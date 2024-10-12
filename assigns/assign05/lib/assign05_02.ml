type 'a tree = 
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let sum_tr t =
  let rec go t cont =
    match t with
    | Leaf -> cont 0
    | Node (x, l, r) -> 
        go l (fun sum_l -> 
          go r (fun sum_r -> 
            cont (x + sum_l + sum_r)))
  in
  go t (fun x -> x)


let my_tree = 
  Node (3, 
    Node (2, 
      Node (1, Leaf, Leaf), 
      Leaf), 
    Node (7, 
      Node (6, Leaf, Leaf), 
      Node (9, Leaf, Leaf)))




