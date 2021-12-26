open Base

type t = { width : int; height : int }

let create width height = { width; height }
let contains t Point.{ x; y } = 0 <= x && x < t.width && 0 <= y && y < t.height

let all t =
  List.range 0 t.height
  |> List.map ~f:(fun y -> List.init t.width ~f:(fun x -> Point.{ x; y }))
  |> List.concat
