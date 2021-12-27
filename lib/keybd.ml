open Base

(** Disable line buffering *)
let cbreak () =
  let open Unix in
  let prev = tcgetattr stdin in
  let info = { prev with c_icanon = false } in
  tcsetattr stdin TCSANOW info;
  prev

(** Disable echo input characters. *)
let noecho () =
  let open Unix in
  let prev = tcgetattr stdin in
  let info = { prev with c_echo = false } in
  tcsetattr stdin TCSANOW info;
  prev

(** Read keyboard input. Default timeout is 0.0 second. *)
let read_key ?(timeoutSec = 0.0) () =
  let buf = Bytes.create 1 in
  let open Unix in
  match select [ stdin ] [] [] timeoutSec with
  | [ _ ], [], [] ->
      if read stdin buf 0 1 = 1 then Some (Bytes.get buf 0) else None
  | _ -> None