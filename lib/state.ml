type t = In_progress | Game_over | Clear

let to_string = function
  | In_progress -> "In Progress"
  | Game_over -> "Game Over"
  | Clear -> "Clear"
