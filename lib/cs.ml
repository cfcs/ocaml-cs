(* Add missing functionality to Cstruct: *)
open Rresult

type t = Cstruct.t

let of_cstruct (cs : Cstruct.t) : t = cs
let to_cstruct (t:t) : Cstruct.t = t

let shift t amount = Cstruct.shift t amount
let pp_hex fmt t = Cstruct.hexdump_pp fmt t
let to_string = Cstruct.to_string
let of_string = Cstruct.of_string
let equal = Cstruct.equal
let exc_sub = Cstruct.sub
let len = Cstruct.len (* TODO consider returning Usane.Uint64.t *)
let create = Cstruct.create
let blit = Cstruct.blit
let concat = Cstruct.concat (*TODO wrap exceptions *)

let exc_set_uint8 buf offset uint8 = Cstruct.set_uint8 buf offset uint8
let exc_get_uint8 buf offset = Cstruct.get_uint8 buf offset
let exc_get_char buf offset = Cstruct.get_char buf offset

module Sensitive : sig
  (** Best-effort sanitizing on finalisation, and avoiding the subset of
      Cstruct that copies the underlying buffer, the goal being to avoid
      leaving sensitive values in the heap.*)

  type t
  val init : int -> (int -> char) -> t
  val make : int -> char -> t
  val create : int -> t
  val to_bytes : t -> bytes
  val to_string : t -> string
  (**  *)
  val equal : t -> t -> bool (** {!Cstruct.equal} *)
  val compare : t -> t -> int (** {!Cstruct.compare} *)
  val set_char : t -> int -> char -> unit (** {!Cstruct.set_char} *)
  val set_uint8 : t -> int -> int -> unit (** {!Cstruct.set_uint8} *)
  val get_char : t -> int -> char (** {!Cstruct.get_char} *)
  val get_uint8 : t -> int -> int (** {!Cstruct.get_uint8} *)
  val substring : t -> int -> int -> string
  (** [substring t offset len] is a substring of [len] bytes from [t],
      starting at [offset].
      The garbage collector (GC) is hooked in an attempt to clear the
      newly allocated string on finalization.
      @raise Invalid_argument if [offset] and [len] do not designate a
      valid segment of [t].
  *)
  val subbytes : t -> int -> int -> bytes
  (** [subbytes t offset len] is the [Bytes.t] equivalent to {!substring}*)
  val copy : t -> int -> int -> string
  (** [copy] is an alias for {!substring}.
      It is provided for compatibility with code relying on {!Cstruct.copy}. *)
  val sub : t -> int -> int -> t
  (** [sub t offset len] is a substring of [t] sharing the underlying buffer.
      It is an alias for {!Cstruct.sub}.*)
  val blit : t -> int -> t -> int -> int -> unit (** {!Cstruct.blit} *)
  val lenv : t list -> int (** {!Cstruct.lenv} *)
  module BE : sig
    (** See {!Cstruct.BE} *)
    val get_uint16 : t -> int -> Cstruct.uint16
    val get_uint32 : t -> int -> Cstruct.uint32
    val get_uint64 : t -> int -> Cstruct.uint64
    val set_uint16 : t -> int -> Cstruct.uint16 -> unit
    val set_uint32 : t -> int -> Cstruct.uint32 -> unit
    val set_uint64 : t -> int -> Cstruct.uint64 -> unit
  end
  module LE : sig
    (** See {!Cstruct.BE} *)
    val get_uint16 : t -> int -> Cstruct.uint16
    val get_uint32 : t -> int -> Cstruct.uint32
    val get_uint64 : t -> int -> Cstruct.uint64
    val set_uint16 : t -> int -> Cstruct.uint16 -> unit
    val set_uint32 : t -> int -> Cstruct.uint32 -> unit
    val set_uint64 : t -> int -> Cstruct.uint64 -> unit
  end
end = struct
  type t = Cstruct.t

  let cstruct_clear_on_finalize (t:t) =
    Gc.finalise (fun cs -> Cstruct.memset cs 0) t

  let init len f =
    let cs = Cstruct.create_unsafe len in
    cstruct_clear_on_finalize cs ;
    for idx = 0 to len -1 do
      Cstruct.set_char cs idx (f idx)
    done ;
    cs

  let make len ch = init len (fun _ -> ch)

  let create len = make len '\000'

  let bytes_clear_on_finalize live =
    live |>
    Gc.finalise (fun dead -> Bytes.fill dead 0 (Bytes.length dead) '\x01')

  let to_bytes t =
    let buf = Bytes.create (Cstruct.len t) in
    (* Try to clear this on finalisation too: *)
    bytes_clear_on_finalize buf;
    Cstruct.blit_to_bytes t 0 buf 0 (Cstruct.len t);
    buf

  let to_string t =
    Bytes.unsafe_to_string (to_bytes t)

  let substring t off len =
    let x = Cstruct.copy t off len in
    Bytes.unsafe_of_string x |> bytes_clear_on_finalize ;
    x

  let copy = substring

  let subbytes t off len =
    substring t off len |> Bytes.unsafe_of_string

  let equal = Cstruct.equal
  let compare = Cstruct.compare
  let check_bounds = Cstruct.check_bounds
  let check_alignment = Cstruct.check_alignment

  let blit = Cstruct.blit

  (* we can't guarantee cleanup of these intermediate values: *)
  let set_char = Cstruct.set_char
  let set_uint8 = Cstruct.set_uint8
  let get_char = Cstruct.get_char
  let get_uint8 = Cstruct.get_uint8
  let sub = Cstruct.sub
  let lenv = Cstruct.lenv
  module BE = Cstruct.BE
  module LE = Cstruct.LE
end

let init len f = String.init len f |> of_string
let make len c = String.make len c |> of_string
let of_char c = make 1 c
let empty = create 0

let iteri_char (f: int -> char -> unit) cs =
  for i = 0 to len cs - 1 do
    f i (Cstruct.get_char cs i)
  done

let iter_char f cs = iteri_char (fun _ -> f) cs

let foldi_char f acc cs =
  let acc = ref acc in
  iteri_char (fun i c -> acc := f i !acc c) cs ;
  !acc

let fold_char f acc cs =
  let acc = ref acc in
  iter_char (fun c -> acc := f !acc c) cs ;
  !acc

let map_char f (t:t) =
  let rec loop acc = function
    | -1 -> acc
    | i -> (loop[@tailcall]) ((f @@ Cstruct.get_char t i)::acc) (pred i)
  in loop [] (max (-1) @@ len t -1)

let dup {Cstruct.buffer ; len ; off} =
  let kind , layout , dim = Bigarray.Array1.(kind, layout, dim) in
  let new_buf =
    Bigarray.Array1.create (kind buffer) (layout buffer) (dim buffer)
  in
  Bigarray.Array1.blit buffer new_buf ;
  Cstruct.of_bigarray ~off ~len new_buf

let wrap_invalid_argument f : ('ok , [> `Msg of string ]) result =
  begin try R.ok @@ f () with
    | Invalid_argument s ->
      Error (`Msg ("Cstruct.invalid_argument: " ^ s))
    | Out_of_memory ->
      Error (`Msg "Cstruct.out_of_memory")
  end

let wrap_f_buf_offset f buf offset =
  wrap_invalid_argument (fun () -> f buf offset)

let wrap_err errval res =
  res |> R.reword_error (function _ -> errval)

let get_uint8 = wrap_f_buf_offset exc_get_uint8
let e_get_uint8 e buf offset = wrap_err e  (get_uint8 buf offset)
let e_set_uint8 e buf offset uint8 =
  (fun () -> exc_set_uint8 buf offset uint8)
  |> wrap_invalid_argument |> wrap_err e


let sub cstr off len =
  wrap_invalid_argument (fun () -> exc_sub cstr off len)

let e_sub e cstr off len =
  wrap_err e (sub cstr off len)

let split_result ?(start=0) buf len =
  wrap_invalid_argument (fun () -> Cstruct.split ~start buf len)

let e_split ?(start=0) e buf len =
  wrap_err e (split_result ~start buf len)

let e_split_char ?(start=0) (e:'error) buf : (char * t, 'error) result =
  (* pops the leftmost char off buf and return the char + remainder *)
  e_split ~start e buf 1 >>| fun (c,tl) -> (Cstruct.get_char c 0 , tl)

let get_char_result buf offset =
  wrap_f_buf_offset Cstruct.get_char buf offset

let e_get_char e buf offset =
  wrap_err e (get_char_result buf offset)

let e_set_char e buf offset c =
  (fun () -> Cstruct.set_char buf offset c)
  |> wrap_invalid_argument |> wrap_err e

let e_blit e src srcoff dst dstoff len =
  (fun () -> Cstruct.blit src srcoff dst dstoff len)
  |> wrap_invalid_argument |> wrap_err e

module BE = struct
  let get_uint16 buf offset =
    wrap_f_buf_offset Cstruct.BE.get_uint16 buf offset
  let get_uint32 buf offset =
    wrap_f_buf_offset Cstruct.BE.get_uint32 buf offset
  let get_uint64 buf offset =
    wrap_f_buf_offset Cstruct.BE.get_uint64 buf offset

  let e_get_uint16 e buf offset =
    wrap_err e (get_uint16 buf offset)
  let e_get_uint32 e buf offset =
    wrap_err e (get_uint32 buf offset)
  let e_get_uint64 e buf offset =
    wrap_err e (get_uint64 buf offset)

  let set_uint16 (buf:t) (offset:int) (int16 : Usane.Uint16.t)
    : (Cstruct.t,[> `Msg of string]) result =
    wrap_invalid_argument (fun () ->
        Cstruct.BE.set_uint16 buf offset int16; buf)
  let set_uint32 buf offset (int32 : Usane.Uint32.t) =
    wrap_invalid_argument (fun () ->
        Cstruct.BE.set_uint32 buf offset int32; buf)
  let set_uint64 buf offset (int64 : Usane.Uint64.t) =
    wrap_invalid_argument (fun () ->
        Cstruct.BE.set_uint64 buf offset int64; buf)

  let e_set_uint16 e buf offset int16 =
    wrap_err e (set_uint16 buf offset int16)
  let e_set_uint32 e buf offset int32 =
    wrap_err e (set_uint32 buf offset int32)
  let e_set_uint64 e buf offset int64 =
    wrap_err e (set_uint64 buf offset int64)

  let create_uint16 (int16 : Usane.Uint16.t) =
    let buf = Cstruct.create 2 in
    Cstruct.BE.set_uint16 buf 0 int16 ; buf
  let create_uint32 (int32 : Usane.Uint32.t) =
    let buf = Cstruct.create 4 in
    Cstruct.BE.set_uint32 buf 0 int32 ; buf
  let create_uint64 (int64 : Usane.Uint64.t) =
    let buf = Cstruct.create 8 in
    Cstruct.BE.set_uint64 buf 0 int64 ; buf

  let e_get_ptimespan32 (e:'e) buf offset : (Ptime.span, 'e) result =
    e_get_uint32 e buf offset
    >>| Int32.to_int >>| Ptime.Span.of_int_s

  let e_get_ptime32 (e:'e) buf offset : (Ptime.t, 'e) result =
    e_get_ptimespan32 e buf offset >>= fun span ->
    match Ptime.of_span span with
    | None -> Error e
    | Some ptime -> Ok ptime

  let e_set_ptimespan32 (e:'e) buf offset ptimespan : (t,'e) result =
    match ptimespan |> Ptime.Span.to_int_s with
    | None -> Error e
    | Some secs -> (* TODO fix negative/positive ints so Ptime_clock
                      won't return stuff >31bit*)
      e_set_uint32 e buf offset (Int32.of_int secs)

  let e_set_ptime32 (e:'e) buf offset ptime : (t,'e) result =
    e_set_ptimespan32 e buf offset (Ptime.to_span ptime)

  let e_create_ptimespan32 e = e_set_ptimespan32 e (create 4) 0
  let e_create_ptime32 e = e_set_ptime32 e (create 4) 0
end

(*
let two_pow_62 = 0x4_000_000_000_000_000L (*no Int64.pow - wtf?*)

let create_tai64_of_ptime time =
  let days, picoseconds = Ptime.to_span time
                          |> Ptime.Span.to_d_ps in
  let seconds = Int64.div picoseconds 1000_000_000_000L in
  Int64.add two_pow_62
    (Int64.add seconds
              (Int64.mul 86400L (Int64.of_int days)))
  |> BE.create_uint64

let create_tai64_n_of_ptime time =
  let _, picoseconds = Ptime.to_span time |> Ptime.Span.to_d_ps in
  let nanosec = Int64.div (Int64.rem picoseconds 1000_000_000_000L) 1000L
             |> Int64.to_int32 in
  concat [create_tai64_of_ptime time; BE.create_uint32 nanosec]

let e_ptime_of_tai64 e buf =
  BE.e_get_uint64 e buf 0 >>= fun raw_int64 ->
  if  raw_int64 < Int64.add two_pow_62 (Int64.of_int32 Int32.min_int)
   || raw_int64 > Int64.add two_pow_62 (Int64.of_int32 Int32.max_int) then
    Error (`Msg "e_ptime_of_tai64, number out of range")
  else
  match    Int64.sub raw_int64 two_pow_62 |> Int64.to_int
        |> Ptime.Span.of_int_s |> Ptime.of_span with
  | None -> Error (`Msg "e_ptime_of_tai64: Ptime.of_span = None")
  | Some ptime -> Ok ptime

let e_ptime_of_tai64_n e buf =
  e_ptime_of_tai64 e buf >>= fun seconds ->
  BE.e_get_uint32 e buf 8 >>= fun raw_nanosec ->
  match Ptime.Span.of_d_ps
          (0, Int64.mul 1000L (* multiply by 1000 to go from ps -> ns*)
             (Int64.of_int32 (Int32.rem raw_nanosec 1_000_000_000l)))
  with
  | None -> Error (`Msg "e_ptime_of_tai64_n: out of range")
  | Some nanoseconds ->
    begin match Ptime.add_span seconds nanoseconds with
      | None -> Error (`Msg "e_ptime_of_tai64_n: add_span")
      | Some ptime -> Ok ptime
    end
*)

let of_hex str =
  try R.ok (Hex.to_string (`Hex str) |> Cstruct.of_string) with
  | Invalid_argument _ -> R.error `Invalid_hex

let to_hex cs =
  match Hex.of_string (Cstruct.to_string cs) with
  | `Hex str -> str

let to_list buf =
  let s = to_string buf in
  let rec loop acc = function
    | -1 -> acc
    | i -> (loop[@tailcall]) (s.[i]::acc) (pred i)
  in loop [] (String.length s -1)

let of_list (lst : char list) =
  let buf = Cstruct.create_unsafe (List.length lst) in
  List.iteri (Cstruct.set_char buf) lst ;
  buf

let make_uint8 int8 =
  let buf = create 1 in
  exc_set_uint8 buf 0 int8 ; buf

let reverse cs : t =
  (* Zarith hides the function for reading little-endian unsigned integers under
     the name "to_bits". Maybe. Probably depends on Sys.big_endian
     In the spirit of wasting time, the author(s) encourages
     kindly doing your own bloody string reversing if you want to
     use Zarith for real-world protocols: *)
  let out_buf = Buffer.create (Cstruct.len cs) in
  (for i = Cstruct.(len cs) - 1 downto 0 do
     Buffer.add_char out_buf Cstruct.(get_char cs i)
   done ;
   Buffer.contents out_buf) |> of_string

(* find char [c] in [b], starting at [offset] and giving up after [max_offset] *)
let index_opt b ?(max_offset) ?(offset=0) c : int option =
  let max = match max_offset with None -> Cstruct.len b | Some m -> (min m Cstruct.(len b)) in
  let rec s = function
    | i when i >= max -> None
    | i when Cstruct.get_char b i = c -> Some i
    | i -> (s[@tailcall]) (i + 1)
  in
  s offset

let e_index e buf ?(max_offset) ?(offset) c =
  (* TODO change buf,c order *)
  R.of_option
    ~none:(fun () -> R.error e)
    (index_opt buf ?max_offset ?offset c)

let index buf ?(max_offset) ?(offset) c : (int, 'error) result =
  e_index (`Msg "Cstruct_invalid_argument") buf ?max_offset ?offset c

(* find substring [needle] in [b] *)
let find b ?(max_offset) ?(offset=0) needle =
  (* TODO label b or needle *)
  let needle_len = Cstruct.len needle
  and b_len = Cstruct.len b in
  let max_offset = match max_offset with
    | None -> max 0 (b_len - needle_len + 1)
    | Some m -> min b_len (m + 1)
  in
  if needle_len = 0 then
    None
  else
    let first_needle = Cstruct.get_char needle 0 in
    let rec next i =
      begin match index_opt ~max_offset ~offset:i b first_needle with
        | None -> None
        | Some c_off ->
          if Cstruct.(equal (exc_sub b c_off needle_len) needle) then
            Some c_off
          else
            (next[@tailcall]) (i + 1)
      end
    in
    next offset

let e_find e b ?max_offset ?offset needle =
  find b ?max_offset ?offset needle
  |> R.of_option ~none:(fun () -> Error e)

let strip_leading_char c buf : t =
  let rec loop offset =
    let max_offset = offset + 1 in
    match index buf ~offset ~max_offset c with
    | Ok _ -> (loop[@tailcall]) max_offset
    | Error _ ->
      e_split (`Msg "Cstruct_invalid_argument") buf offset
      |> R.get_ok
      |> fun (_, tl) -> tl
  in
  loop 0

let strip_trailing_char c buf : t =
  strip_leading_char c (reverse buf) |> reverse

let split_by_char c ?offset ?max_offset buf : (t*t, 'error) result =
  begin match index_opt ?offset ?max_offset buf c with
    | None -> Ok (Cstruct.create 0 , buf)
    | Some i -> split_result buf i
  end

let equal_string str buf = equal buf (of_string str)

let e_equal_string e str buf =
  (* aka: Types.e_bool e (equal_string str buf) *)
  match equal_string str buf with
  | true -> Ok ()
  | false -> Error e

let e_is_empty (e:'e) buf : (unit, 'e) result = if 0 = len buf then Ok () else Error e

let e_find_list e buf_list buf : (t,'error) result =
  (* TODO perhaps the "find" name is a bit confusing here. this is "find" in the sense of List.find, not Cs.find*)
  match List.find (equal buf) buf_list with
  | exception Not_found -> Error e
  | member -> Ok member

let e_find_string_list e str_list buf : (string,'error) result=
  e_find_list e (List.map of_string str_list) buf
  >>| to_string

let next_line ?max_length buf : [> `Last_line of t | `Next_tuple of t*t] =
  begin match index_opt ?max_offset:max_length buf '\n' with
    | None -> `Last_line buf
    | Some 0 -> `Next_tuple (Cstruct.create 0 , exc_sub buf 1 (len buf -1))
    | Some n_idx when Cstruct.get_char buf (n_idx-1) = '\r' ->
      `Next_tuple (
        exc_sub buf 0 (n_idx-1),
        exc_sub buf (n_idx+1) (len buf -n_idx-1)
      )
    | Some n_idx ->
      `Next_tuple (
        exc_sub buf 0 n_idx ,
        exc_sub buf (n_idx+1) (len buf - n_idx-1)
      )
  end

let xor a b =
  let a_len = len a in
  if a_len <> len b then
    R.error_msgf "cannot xor; got lengths %d and %d" a_len (len b)
  else Ok (init a_len (fun i ->
      Char.chr @@ (exc_get_uint8 a i) lxor (exc_get_uint8 b i) ))

module W = struct
  type wt = t ref
  let increase t n_len : int =
    let old_len = Cstruct.len !t in
    ( try t := Cstruct.add_len !t n_len
      with Invalid_argument _ ->
        t := Cstruct.set_len
            (concat [!t ; (* amortize expansion cost: *)
                     create (n_len + Bigarray.Array1.dim !t.Cstruct.buffer) ]
            ) (old_len + n_len) ;
    ) ;
    old_len

  let to_string t = to_string !t
  let to_cs t = !t

  let create initial_len =
    ref (Cstruct.set_len (Cstruct.create_unsafe initial_len) 0)

  let of_cs cs = ref (dup cs)

  let char t c = Cstruct.set_char !t (increase t 1) c

  let min_len cs =
    let src_len = match cs with
      | `Cs cs -> Cstruct.len cs
      | `Str str -> String.length str
    in function
       | None -> src_len
       | Some len -> min len src_len

  let cs t ?(offset=0) ?len src =
    let src_len = min_len (`Cs src) len in
    Cstruct.blit src offset !t (increase t src_len) src_len

  let uint8  t i = exc_set_uint8 !t (increase t 1) i
  let uint16 t i = cs t (BE.create_uint16 i)
  let uint32 t i = cs t (BE.create_uint32 i)
  let uint64 t i = cs t (BE.create_uint64 i)

  let string t ?(offset=0) ?len src =
    let src_len = min_len (`Str src) len in
    Cstruct.blit_from_string src offset !t (increase t src_len) src_len

  let e_ptime32 e t ptime = BE.e_create_ptime32 e ptime >>| cs t
  let e_ptimespan32 e t ptimespan =
    BE.e_create_ptimespan32 e ptimespan >>| cs t
end

module R =
struct
  type 'e rt = { cs: t
              ; mutable off : int
              ; err: 'e}
  let len r = len r.cs - r.off
  let pp ppf r =
    Fmt.pf ppf "Cs.R @[<v>off: %d len: %d@, consumed: %a@,remaining: %a@]"
      r.off (len r)
      Cstruct.hexdump_pp (exc_sub r.cs 0 r.off)
      Cstruct.hexdump_pp (exc_sub r.cs r.off (len r))
  let of_cs err ?(offset=0) cs = {cs ; off = offset; err}
  let of_string err ?(offset=0) str =  of_cs err ~offset (of_string str)
  let alen (r:'e rt) adjustment (rv:('ok,'e) result) : ('ok,'e) result =
    rv >>| fun v -> r.off <- (r.off + adjustment) ; v
  let char r   =    e_get_char   r.err r.cs r.off |> alen r 1

  let uint8 r  =    e_get_uint8  r.err r.cs r.off |> alen r 1
  let uint16 r = BE.e_get_uint16 r.err r.cs r.off |> alen r 2
  let uint32 r = BE.e_get_uint32 r.err r.cs r.off |> alen r 4
  let uint64 r = BE.e_get_uint64 r.err r.cs r.off |> alen r 4

  let cs r len = sub r.cs r.off len
                 |> R.reword_error (fun _ -> r.err)
                 |> alen r len
  let string r len = cs r len >>| to_string
  let string_z r len = cs r len >>| strip_trailing_char '\x00' >>| to_string
  let equal_string r s = cs r (String.length s) >>= e_equal_string r.err s
end

module AnnotR = struct

  type operation =
    | O_char
    | O_uint8
    | O_uint16
    | O_uint32
    | O_uint64
    | O_cs
    | O_string
    | O_stringz

  type entry =
    | Section of section
    | Entry of annotation
    | Product of unit Fmt.t
  and section =
    { header: unit Fmt.t;
      entries : entry Queue.t;
      parent: section ;
    }
  and annotation = {
      op: operation;
      pp: (Format.formatter -> operation -> unit);
      off: int;
      len: int;
    }

  type state = {
    rt: entry Queue.t R.rt ;
    root : section ;
    current: section ref ;
  }

  type 'ok retval = ('ok, state) Rresult.result

  (*let pp ppf state =
    Queue.iter (function
          Entry -annot ->
        Fmt.pf ppf "%a" annot.pp annot.op
      ) state.root.entries*)

  (* Initialization functions *)
  let of_cs = R.of_cs
  let of_string = R.of_string

  (* Read-only *)
  let len = R.len

  let annotate_section ~pp (state:state) =
    let this = {entries = Queue.create () ;
                parent = !(state.current) ;
                header = pp; } in
    Queue.push (Section this) !(state.current).entries ;
    state.current := this

  let exit_section ~pp state =
    Queue.push (Product pp) !(state.current).entries ;
    state.current := !(state.current).parent

  let annotate ~pp ~pp_error (state:state) operation cb : 'a retval =
    let orig_off = state.rt.R.off in
    let retval = cb state.rt in
    begin match retval with
      | Error _ -> Error state
      | Ok retval ->
        let len = state.rt.R.off - orig_off in
        Queue.add (Entry {
          off = orig_off ;
          len ;
          op = operation ;
          pp ;
        }) !(state.current).entries ;
        Ok retval
    end

  let char ~pp ~pp_error state =
    annotate ~pp ~pp_error state O_char R.char

  let uint8 ~pp ~pp_error state =
    annotate ~pp ~pp_error state O_uint8 R.uint8

  let uint16 ~pp ~pp_error state =
    annotate ~pp ~pp_error state O_uint16 R.uint16
  let uint32 ~pp ~pp_error state =
    annotate ~pp ~pp_error state O_uint32 R.uint32
  let uint64 ~pp ~pp_error state =
    annotate ~pp ~pp_error state O_uint64 R.uint64

  let cs ~pp ~pp_error state len =
    annotate ~pp ~pp_error state O_cs (fun x -> R.cs x len)

  let string ~pp ~pp_error state len =
    annotate ~pp ~pp_error state O_string (fun x -> R.string x len)

  let string_z ~pp ~pp_error state len =
    annotate ~pp ~pp_error state O_string (fun x -> R.string_z x len)
end
