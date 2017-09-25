open Rresult
type t = Cstruct.t

val to_string : t -> string
val equal : t -> t -> bool
val sub : t -> int -> int -> t
val len : t -> int
val of_string : ?allocator:(int -> t) -> string -> t
val create : int -> t
val blit : t -> int -> t -> int -> int -> unit
val concat : t list -> t
val set_uint8 : t -> int -> Usane.Uint8.t -> unit
val of_char : char -> t
val dup : t -> t
val get_uint8_result : t -> int -> (Usane.Uint8.t, [> R.msg ]) result
val e_get_uint8 : 'error -> t -> int -> (Usane.Uint8.t, 'error) result
val sub_result : t -> int -> int -> (t, [> R.msg ]) result
val e_sub : 'error -> t -> int -> int -> (t, 'error) result
val split_result : ?start:int -> t -> int -> (t * t, [> R.msg ]) result
val e_split : ?start:int -> 'error -> t -> int -> (t * t, 'error) result
val e_split_char : ?start:int -> 'error -> t -> (char * t, 'error) result
val get_char_result : t -> int -> (char, [> R.msg ]) result
val e_get_char : 'error -> t -> int -> (char, 'error) result
val e_set_char : 'error -> t -> int -> char -> (unit, 'error) result
val e_blit : 'error -> t -> int -> t -> int -> int -> (unit, 'error) result

module BE :
  sig
    val get_uint16 :
      t -> int -> (Usane.Uint16.t, [> R.msg ]) result
    val e_get_uint16 :
      'error -> t -> int -> (Usane.Uint16.t, 'error) result
    val get_uint32 :
      t -> int -> (Usane.Uint32.t, [> R.msg ]) result
    val e_get_uint32 :
      'error -> t -> int -> (Usane.Uint32.t, 'error) result
    val set_uint16 :
      t -> int -> Usane.Uint16.t -> (t, [> R.msg ]) result
    val e_set_uint16 :
      'error -> t -> int -> Usane.Uint16.t -> (t, 'error) result
    val create_uint16 : Usane.Uint16.t -> t
    val set_uint32 :
      t -> int -> Usane.Uint32.t -> (t, [> R.msg ]) result
    val e_set_uint32 :
      'error -> t -> int -> Usane.Uint32.t -> (t, 'error) result
    val create_uint32 : Usane.Uint32.t -> t
    val e_get_ptimespan32 :
      'error -> t -> int -> (Ptime.span, 'error) result
    (** [e_get_ptime32 e buf offset] is the big-endian UNIX timestamp contained
        in [buf] at [offset], or [Error e] *)

    val e_get_ptime32 :
      'error -> t -> int -> (Ptime.t, 'error) result
    val e_set_ptimespan32 :
      'error -> t -> int -> Ptime.span -> (t, 'error) result
    val e_set_ptime32 : 'error -> t -> int -> Ptime.t -> (t, 'error) result
    val e_create_ptimespan32 : 'error -> Ptime.span -> (t, 'error) result
    val e_create_ptime32 : 'error -> Ptime.t -> (t, 'error) result
  end
val of_hex : string -> (t, [> `Invalid_hex ]) result
val to_hex : t -> string
val to_list : t -> char list
val of_list : char list -> t
val make_uint8 : Usane.Uint8.t -> t
val reverse : t -> t
val index_opt : t -> ?max_offset:int -> ?offset:int -> char -> int option
val e_index :
  'error -> t -> ?max_offset:int -> ?offset:int -> char -> (int, 'error) result
val index :
  t -> ?max_offset:int -> ?offset:int -> char -> (int, [> R.msg ]) result
val find : t -> ?max_offset:int -> ?offset:int -> t -> int option
val e_find :
  'error -> t -> ?max_offset:int -> ?offset:int -> t -> (int, 'error) result
val strip_leading_char : char -> t -> t
val strip_trailing_char : char -> t -> t
val split_by_char :
  char -> ?offset:int -> ?max_offset:int -> t -> (t * t, [> R.msg ]) result
val equal_string : string -> t -> bool
val e_equal_string : 'error -> string -> t -> (unit, 'error) result
val e_is_empty : 'error -> t -> (unit, 'error) result
val e_find_list : 'error -> t list -> t -> (t, 'error) result
val e_find_string_list : 'error -> string list -> t -> (string, 'error) result
val next_line :
  ?max_length:int -> t -> [> `Last_line of t | `Next_tuple of t * t ]

module W :
  sig
    type t
    val increase : t -> int -> int
    val to_string : t -> string
    val to_cs : t -> Cstruct.t
    val of_cs : Cstruct.t -> t
    val create : int -> t
    val char : t -> char -> unit
    val cs : t -> ?offset:int -> ?len:int -> Cstruct.t -> unit
    val uint16 : t -> Usane.Uint16.t -> unit
    val uint32 : t -> Usane.Uint32.t -> unit
    val str : t -> ?offset:int -> ?len:int -> string -> unit
    val e_ptime32 :
      'error -> t -> Ptime.t -> (t, 'error) result
    val e_ptimespan32 :
      'error -> t -> Ptime.span -> (t, 'error) result
  end
