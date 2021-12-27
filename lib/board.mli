type t = { width : int; height : int }

val create : width:int -> height:int -> t
(* Create board. *)

val contains : t -> Point.t -> bool
(* Whether board t containts point or not. *)

val all : t -> Point.t list
(* All points in board t. *)
