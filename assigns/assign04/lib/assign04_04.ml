type ident = string

type expr' =
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' =
  | Int
  | Bool

type context = (ident * ty') list

let rec lookup ctx id =
  match ctx with
  | [] -> None
  | (name, t) :: rest ->
      if name = id then Some t
      else lookup rest id

let rec type_of' ctx e =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Var id -> lookup ctx id
  | Add (e1, e2) ->
      (match type_of' ctx e1, type_of' ctx e2 with
       | Some Int, Some Int -> Some Int
       | _ -> None)
  | Or (e1, e2) ->
      (match type_of' ctx e1, type_of' ctx e2 with
       | Some Bool, Some Bool -> Some Bool
       | _ -> None)
  | IfThenElse (e1, e2, e3) ->
      (match type_of' ctx e1, type_of' ctx e2, type_of' ctx e3 with
       | Some Bool, Some t2, Some t3 when t2 = t3 -> Some t2
       | _ -> None)
  | Let (id, e1, e2) ->
      (match type_of' ctx e1 with
       | Some t1 -> type_of' ((id, t1) :: ctx) e2
       | None -> None)
