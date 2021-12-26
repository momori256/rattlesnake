open Base

module Direction : sig
  type t = Up | Right | Down | Left

  val next_pos : Point.t -> t -> Point.t
  val of_char : char -> t option
end = struct
  type t = Up | Right | Down | Left

  let next_pos point dir =
    match dir with
    | Up -> Point.{ point with y = point.y - 1 }
    | Right -> Point.{ point with x = point.x + 1 }
    | Down -> Point.{ point with y = point.y + 1 }
    | Left -> Point.{ point with x = point.x - 1 }

  let of_char = function
    | 'w' -> Some Up
    | 'd' -> Some Right
    | 's' -> Some Down
    | 'a' -> Some Left
    | _ -> None
end

type t = { positions : Point.t list; direction : Direction.t; grow : bool }

let create ?(point = Point.{ x = 0; y = 0 }) ?(len = 3)
    ?(direction = Direction.Right) () =
  let positions =
    List.init len ~f:(fun x -> Point.{ x = point.x + x; y = point.y + 0 })
    |> List.rev
  in
  { positions; direction; grow = false }

let head t = List.hd_exn t.positions
let positions t = t.positions
let eat t = { t with grow = true }

let step t =
  let hd = Direction.next_pos (head t) t.direction in
  let tail =
    match t.grow with
    | true -> t.positions
    | false -> List.take t.positions (List.length t.positions - 1)
  in
  if List.mem tail hd ~equal:Point.equal then None
  else Some { t with positions = hd :: tail; grow = false }
