val cbreak : unit -> Unix.terminal_io
(* Disable line buffering *)

val noecho : unit -> Unix.terminal_io
(* Disable echo input characters. *)

val read_key : ?timeoutSec:float -> unit -> char option
(* Read keyboard input. Default timeout is 0.0 second. *)
