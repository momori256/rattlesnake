open Base

type t = { main : Curses.window; sub : Curses.window }

let create Board.{ width; height } =
  let _ = Curses.initscr () in
  let main = Curses.newwin (height + 2) (width + 2) 0 0 in
  let sub = Curses.newwin 3 (width + 2) (height + 2) 0 in
  { main; sub }

let finilize () = Curses.endwin ()
let refresh () = if Curses.refresh () then () else failwith "Failed to refresh"
let wrefresh t = if Curses.wrefresh t then () else failwith "Failed to wrefresh"

let draw t game =
  Curses.wclear t.main;
  Curses.wclear t.sub;
  Curses.box t.main 0 0;
  let print_exn Point.{ x; y } ch =
    match Curses.mvwaddch t.main (y + 1) (x + 1) (Char.to_int ch) with
    | false -> failwith "Failed to print char."
    | true -> ()
  in
  Game.snake_positions game
  |> List.iter ~f:(fun point ->
         let ch =
           if Point.equal point (Game.snake_head game) then 'o' else '.'
         in
         print_exn point ch);
  print_exn (Game.apple game) 'O';
  wrefresh t.main;
  Curses.box t.sub 0 0;
  let _ = Curses.mvwaddstr t.sub 1 1 (Game.msg game) in
  wrefresh t.sub
