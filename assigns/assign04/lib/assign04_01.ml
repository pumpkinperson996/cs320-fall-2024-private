let rec lifespan f start pred =
  if pred start then 0
  else 1 + lifespan f (f start) pred

let last_function_standing funcs start pred =
  let rec find_max lst current_max max_fn =
    match lst with
    | [] -> max_fn
    | f :: fs ->
        let l = lifespan f start pred in
        if l > current_max then
          find_max fs l (Some f)
        else if l = current_max then
          find_max fs l None
        else
          find_max fs current_max max_fn
  in
  match funcs with
  | [] -> None
  | _ -> find_max funcs (-1) None
