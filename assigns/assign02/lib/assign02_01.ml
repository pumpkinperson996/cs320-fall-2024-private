type piece = X | O

type pos = Piece of piece | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = Top | Middle | Bottom
type col_index = Left | Middle | Right
type pos_index = row_index * col_index

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

let get_pos (b: board) ((r,c): pos_index): pos =
  let (r1, r2, r3) = b in
  let row = match r with
    | Top -> r1
    | Middle -> r2
    | Bottom -> r3
  in
  match c with
    | Left -> fst3 row
    | Middle -> snd3 row
    | Right -> trd3 row

let all_same = function
  | (Piece X, Piece X, Piece X) -> true
  | (Piece O, Piece O, Piece O) -> true
  | _ -> false

let rec exists f lst =
  match lst with
  | [] -> false
  | x :: xs -> if f x then true else exists f xs

let winner (b: board): bool =
  let (r1, r2, r3) = b in
  let rows = [r1; r2; r3] in
  let cols = [
    (fst3 r1, fst3 r2, fst3 r3);
    (snd3 r1, snd3 r2, snd3 r3);
    (trd3 r1, trd3 r2, trd3 r3)
  ] in
  let diagonals = [
    (fst3 r1, snd3 r2, trd3 r3);
    (trd3 r1, snd3 r2, fst3 r3)
  ] in
  exists all_same (rows @ cols @ diagonals)
