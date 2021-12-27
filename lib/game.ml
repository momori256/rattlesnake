open Base

type t = { board : Board.t; snake : Snake.t; apple : Point.t; state : State.t }

let board t = t.board
let snake_positions t = Snake.positions t.snake
let snake_head t = Snake.head t.snake
let apple t = t.apple
let state t = t.state
let set_direction t direction = { t with snake = { t.snake with direction } }

let create_apple_randomly board snake =
  let positions = Snake.positions snake in
  let candidates =
    Board.all board
    |> List.filter ~f:(fun p -> not (List.mem positions p ~equal:Point.equal))
  in
  List.random_element candidates

let create ~width ~height =
  let board = Board.create ~width ~height in
  let snake = Snake.create () in
  if
    not
      (List.for_all (Snake.positions snake) ~f:(fun p -> Board.contains board p))
  then None
  else
    match create_apple_randomly board snake with
    | None -> None
    | Some apple -> Some { board; snake; apple; state = State.In_progress }

let update t =
  match Snake.step t.snake with
  | None -> { t with state = Game_over }
  | Some snake ->
      let hd = Snake.head snake in
      if not (Board.contains t.board hd) then { t with state = Game_over }
      else if Point.equal hd t.apple then
        let snake = Snake.eat snake in
        match create_apple_randomly t.board snake with
        | None -> { t with state = Clear }
        | Some apple -> { t with snake; apple }
      else { t with snake }

let msg t =
  match t.state with
  | In_progress ->
      Printf.sprintf "length: %d / %d"
        (List.length (Snake.positions t.snake))
        (List.length (Board.all t.board))
  | Game_over | Clear -> State.to_string t.state
