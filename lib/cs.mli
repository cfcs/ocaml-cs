type t
val pp_hex : Format.formatter -> t -> unit
val empty : t
val to_string : t -> string
val of_string : ?allocator:(int -> t) -> string -> t
val init : int -> (int -> char) -> t
val make : int -> char -> t
val equal : t -> t -> bool
val len : t -> int
val create : int -> t
val blit : t -> int -> t -> int -> int -> unit
val concat : t list -> t
val set_uint8 : t -> int -> Usane.Uint8.t -> unit
val of_char : char -> t
val dup : t -> t
val get_uint8_unsafe : t -> int -> Usane.Uint8.t
val get_uint8 :
  t -> int -> (Usane.Uint8.t, [> Rresult.R.msg ]) Rresult.result
val e_get_uint8 :
  'error -> t -> int -> (Usane.Uint8.t, 'error) Rresult.result
val sub_unsafe : t -> int -> int -> t
val sub : t -> int -> int -> (t, [> Rresult.R.msg ]) Rresult.result
val e_sub : 'error -> t -> int -> int -> (t, 'error) Rresult.result
val split_result :
  ?start:int -> t -> int -> (t * t, [> Rresult.R.msg ]) Rresult.result
val e_split :
  ?start:int -> 'error -> t -> int -> (t * t, 'error) Rresult.result
val e_split_char :
  ?start:int -> 'error -> t -> (char * t, 'error) Rresult.result
val get_char_result : t -> int -> (char, [> Rresult.R.msg ]) Rresult.result
val e_get_char : 'error -> t -> int -> (char, 'error) Rresult.result
val e_set_char : 'error -> t -> int -> char -> (unit, 'error) Rresult.result
val e_blit :
  'error -> t -> int -> t -> int -> int -> (unit, 'error) Rresult.result
val create_tai64_of_ptime : Ptime.t -> t
val create_tai64_n_of_ptime : Ptime.t -> t
val e_ptime_of_tai64 :
  ([> `Msg of string ] as 'error) -> t -> (Ptime.t, 'error) Rresult.result
val e_ptime_of_tai64_n :
  ([> `Msg of string ] as 'error) -> t -> (Ptime.t, 'error) Rresult.result
module BE :
  sig
    val get_uint16 :
      t -> int -> (Usane.Uint16.t, [> Rresult.R.msg ]) Rresult.result
    val e_get_uint16 :
      'error -> t -> int -> (Usane.Uint16.t, 'error) Rresult.result
    val get_uint32 :
      t -> int -> (Usane.Uint32.t, [> Rresult.R.msg ]) Rresult.result
    val e_get_uint32 :
      'error -> t -> int -> (Usane.Uint32.t, 'error) Rresult.result
    val set_uint16 :
      t -> int -> Usane.Uint16.t -> (t, [> Rresult.R.msg ]) Rresult.result
    val e_set_uint16 :
      'error -> t -> int -> Usane.Uint16.t -> (t, 'error) Rresult.result
    val create_uint16 : Usane.Uint16.t -> t
    val set_uint32 :
      t -> int -> Usane.Uint32.t -> (t, [> Rresult.R.msg ]) Rresult.result
    val e_set_uint32 :
      'error -> t -> int -> Usane.Uint32.t -> (t, 'error) Rresult.result
    val create_uint32 : Usane.Uint32.t -> t
    val create_uint64 : Usane.Uint64.t -> t
    val e_get_ptimespan32 :
      'error -> t -> int -> (Ptime.span, 'error) Rresult.result
    val e_get_ptime32 :
      'error -> t -> int -> (Ptime.t, 'error) Rresult.result
    val e_set_ptimespan32 :
      'error -> t -> int -> Ptime.span -> (t, 'error) Rresult.result
    val e_set_ptime32 :
      'error -> t -> int -> Ptime.t -> (t, 'error) Rresult.result
    val e_create_ptimespan32 :
      'error -> Ptime.span -> (t, 'error) Rresult.result
    val e_create_ptime32 : 'error -> Ptime.t -> (t, 'error) Rresult.result
  end
val of_hex : string -> (t, [> `Invalid_hex ]) Rresult.result
val to_hex : t -> string
val to_list : t -> char list
val of_list : char list -> t
val make_uint8 : Usane.Uint8.t -> t
val reverse : t -> t
val index_opt : t -> ?max_offset:int -> ?offset:int -> char -> int option
val e_index :
  'error ->
  t -> ?max_offset:int -> ?offset:int -> char -> (int, 'error) Rresult.result
val index :
  t ->
  ?max_offset:int ->
  ?offset:int -> char -> (int, [> Rresult.R.msg ]) Rresult.result
val find : t -> ?max_offset:int -> ?offset:int -> t -> int option
val e_find :
  'error ->
  t -> ?max_offset:int -> ?offset:int -> t -> (int, 'error) Rresult.result
val strip_leading_char : char -> t -> t
val strip_trailing_char : char -> t -> t
val split_by_char :
  char ->
  ?offset:int ->
  ?max_offset:int -> t -> (t * t, [> Rresult.R.msg ]) Rresult.result
val equal_string : string -> t -> bool
val e_equal_string : 'error -> string -> t -> (unit, 'error) Rresult.result
val e_is_empty : 'error -> t -> (unit, 'error) Rresult.result
val e_find_list : 'error -> t list -> t -> (t, 'error) Rresult.result
val e_find_string_list :
  'error -> string list -> t -> (string, 'error) Rresult.result
val next_line :
  ?max_length:int -> t -> [> `Last_line of t | `Next_tuple of t * t ]

val xor : t -> t -> (t, [> Rresult.R.msg] ) result
(** [xor a b] returns a new [t] that is the bitwise XOR of [a] and [b].
    Fails if [a] and [b] are not of the same length.
 *)

module W :
  sig
    type wt
    val increase : wt -> int -> int
    val to_string : wt -> string
    val to_cs : wt -> t
    val of_cs : t -> wt
    val create : int -> wt
    val char : wt -> char -> unit
    val cs : wt -> ?offset:int -> ?len:int -> t -> unit
    val uint16 : wt -> Usane.Uint16.t -> unit
    val uint32 : wt -> Usane.Uint32.t -> unit
    val str : wt -> ?offset:int -> ?len:int -> string -> unit
    val e_ptime32 : 'error -> wt -> Ptime.t -> (unit, 'error) Rresult.result
    val e_ptimespan32 :
      'error -> wt -> Ptime.span -> (unit, 'error) Rresult.result
  end
module R :
  sig
    type 'error rt
    val of_cs : 'error -> ?offset:int -> t -> 'error rt
    val of_string : 'error -> ?offset:int -> string -> 'error rt
    val char : 'error rt -> (char, 'error) Rresult.result
    val uint8 : 'error rt -> (Usane.Uint8.t, 'error) Rresult.result
    val uint16 : 'error rt -> (Usane.Uint16.t, 'error) Rresult.result
    val uint32 : 'error rt -> (Usane.Uint32.t, 'error) Rresult.result
    val cs : 'error rt -> int -> (t, 'error) Rresult.result
    val string : 'error rt -> int -> (string, 'error) Rresult.result
    val string_z : 'error rt -> int -> (string, 'error) Rresult.result
    val equal_string : 'error rt -> string -> (unit, 'error) Rresult.result
    val len : 'error rt -> int
    val pp : Format.formatter -> 'error rt -> unit
  end
