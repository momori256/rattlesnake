type t

val create : width:int -> height:int -> t option
(* Create game. *)

val board : t -> Board.t
val snake_positions : t -> Point.t list
val snake_head : t -> Point.t
val apple : t -> Point.t
val state : t -> State.t
(* Getter. *)

val set_direction : t -> Snake.Direction.t -> t
(* Setter. *)

val update : t -> t
val msg : t -> string
