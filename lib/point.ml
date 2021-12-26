type t = { x : int; y : int }

let create x y = { x; y }
let equal p1 p2 = p1.x = p2.x && p1.y = p2.y
