let is_prime n = 
  if n <= 1 then false
  else
    let rec aux d =
      if d * d > n then true
      else if n mod d = 0 then false
      else aux (d + 1)
    in aux 2
  