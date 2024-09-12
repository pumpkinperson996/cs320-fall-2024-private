  let is_prime n = 
    if n <= 1 then false
    else
      let rec aux d =
        if d * d > n then true
        else if n mod d = 0 then false
        else aux (d + 1)
      in aux 2
      
  
  let rec find_nth_prime current found target =
    if is_prime current then
      if found = target then current
      else find_nth_prime (current + 1) (found + 1) target
    else find_nth_prime (current + 1) found target
  
  let nth_prime n = find_nth_prime 2 0 n
  
  