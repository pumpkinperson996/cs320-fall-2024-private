type ident = string

type ty = 
  | Unit                     
  | Arr of ty * ty            

type expr = 
  | Var of ident             
  | Fun of ident * ty * expr  
  | App of expr * expr        

type ctxt = (ident * ty) list

let rec lookup_type x gamma = 
  match gamma with
  | [] -> None                
  | (y, t) :: rest -> 
      if x = y then Some t else lookup_type x rest

let rec type_of gamma e =
  match e with
  | Var x -> lookup_type x gamma     
  | Fun (x, t, e_body) -> 
      let gamma' = (x, t) :: gamma in
      (match type_of gamma' e_body with
       | Some t_body -> Some (Arr (t, t_body))  
       | None -> None)
  | App (e1, e2) -> 
      (match type_of gamma e1, type_of gamma e2 with
       | Some (Arr (t_arg, t_res)), Some t_e2 when t_arg = t_e2 -> Some t_res
       | _ -> None)  


let expr_example = 
  App (
    Fun ("f", Arr (Unit, Unit), Fun ("x", Unit, App (Var "f", Var "x"))),
    Fun ("y", Unit, Var "y")
  )



