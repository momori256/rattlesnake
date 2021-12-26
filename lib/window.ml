open Base

type t = Curses.window

let create Board.{ width; height } =
  let _ = Curses.initscr () in
  Curses.newwin (height + 2) (width + 2) 0 0

let finilize () = Curses.endwin ()
let refresh () = if Curses.refresh () then () else failwith "Failed to refresh"
let wrefresh t = if Curses.wrefresh t then () else failwith "Failed to wrefresh"

let draw t snake apple =
  Curses.wclear t;
  Curses.box t 0 0;
  (* refresh (); *)
  let print_exn Point.{ x; y } ch =
    match Curses.mvwaddch t (y + 1) (x + 1) (Char.to_int ch) with
    | false -> failwith "Failed to print char."
    | true -> ()
  in
  Snake.positions snake |> List.iter ~f:(fun point -> print_exn point 's');
  print_exn apple 'a';
  let _ = Curses.mvcur 100 100 100 100 in
  wrefresh t

let draw_msg t msg = if Curses.mvwaddstr t 2 2 msg then wrefresh t else ()
