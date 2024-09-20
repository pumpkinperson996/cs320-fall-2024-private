type matrix = {
  entries : float list list;  
  rows : int;
  cols : int;
}


let rec split_at n lst =
  match (n, lst) with
  | (0, _) -> ([], lst)
  | (_, []) -> ([], [])
  | (n, x :: xs) ->
    let (first_part, rest) = split_at (n - 1) xs in
    (x :: first_part, rest)

let rec split_into_rows lst n =
  match lst with
  | [] -> []
  | _ ->
    let row, rest = split_at n lst in
    row :: split_into_rows rest n

let mk_matrix (entries_list: float list) ((r, c): int * int) : matrix =
  let rows = split_into_rows entries_list c in
  { entries = rows; rows = r; cols = c }
