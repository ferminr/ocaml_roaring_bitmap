(** Imperative bitmaps of unsigned32 ints *)

type t
val create : ?n:int -> unit -> t
val empty : unit -> t
val is_empty : t -> bool
val stats :
  t -> (string * int) * (string * int) * (string * int) * (string * int)
val mem : t -> int -> bool
val add : t -> int -> unit
val remove : t -> int -> unit
val of_list : int list -> t
val of_array : int array -> t
val of_seq : int Seq.t -> t

(** {1 Iterators} *)

val iter : (int -> unit) -> t -> unit
val fold : ('a -> int -> 'a) -> t -> 'a -> 'a
val to_seq : t -> int Seq.t
