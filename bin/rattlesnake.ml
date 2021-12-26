open Base
open Lib

type t = { game : Game.t; window : Window.t }

let draw t = Window.draw t.window t.game.snake t.game.apple

let rec run t prev =
  let t =
    match Keybd.read_key () with
    | None -> t
    | Some key -> (
        match Snake.Direction.of_char key with
        | None -> t
        | Some dir -> { t with game = Game.set_direction t.game dir })
  in
  let now = Unix.gettimeofday () in
  if Float.(now - prev < 0.25) then (
    Unix.sleepf 0.01;
    run t prev)
  else
    let game = Game.update t.game in
    let t = { t with game } in
    draw t;
    match Game.get_state game with
    | In_progress -> run t now
    | Game_over -> false
    | Clear -> true

let () =
  let prev_terminfo = Keybd.cbreak () in
  let _ = Keybd.noecho () in
  match Game.create ~width:20 ~height:10 with
  | None -> failwith "Invalid paramters."
  | Some game ->
      let window = Window.create game.board in
      let t = { game; window } in
      (match run t (Unix.gettimeofday ()) with
      | true -> Window.draw_msg t.window "You Win"
      | false -> Window.draw_msg t.window "Game Over");
      let _ = Curses.getch () in
      let _ = Window.finilize () in
      Unix.(tcsetattr stdin TCSANOW prev_terminfo);
      ()
