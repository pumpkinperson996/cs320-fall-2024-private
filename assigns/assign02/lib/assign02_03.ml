type dir = North | South | East | West

type path = dir list

let move (x, y) direction =
  match direction with
  | North -> (x, y + 1)
  | South -> (x, y - 1)
  | East  -> (x + 1, y)
  | West  -> (x - 1, y)

let dist (p: path): float =
  let (final_x, final_y) = List.fold_left move (0, 0) p in
  sqrt (float_of_int (final_x * final_x + final_y * final_y))
