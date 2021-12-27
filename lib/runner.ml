open Base

module Terminal = struct
  type t = { info : Unix.terminal_io }

  let create () =
    let info = Unix.(tcgetattr stdin) in
    let _ = Keybd.cbreak () in
    let _ = Keybd.noecho in
    { info }

  let restore t = Unix.(tcsetattr stdin TCSANOW t.info)
end

module Freq = struct
  type t = Low | Middle | High

  let of_level = function 0 -> Low | 1 -> Middle | _ -> High
  let to_float = function Low -> 1.0 | Middle -> 0.25 | High -> 0.1
end

type t = { game : Game.t; window : Window.t; term : Terminal.t; freq : Freq.t }

let rec main_loop t prev =
  let t =
    match Keybd.read_key () with
    | None -> t
    | Some key -> (
        match Snake.Direction.of_char key with
        | None -> t
        | Some dir -> { t with game = Game.set_direction t.game dir })
  in
  let now = Unix.gettimeofday () in
  if Float.(now - prev < Freq.to_float t.freq) then (
    Unix.sleepf 0.01;
    main_loop t prev)
  else
    let game = Game.update t.game in
    let t = { t with game } in
    Window.draw t.window t.game;
    match Game.state game with
    | In_progress -> main_loop t now
    | Game_over -> ()
    | Clear -> ()

let finalize t =
  let _ = Window.finilize () in
  Terminal.restore t.term

let run ~width ~height ~level =
  let term = Terminal.create () in
  match Game.create ~width ~height with
  | None -> failwith "Invalid paramters."
  | Some game ->
      let window = Window.create (Game.board game) in
      let t = { game; window; term; freq = Freq.of_level level } in
      main_loop t (Unix.gettimeofday ());
      Unix.sleep 5;
      finalize t
