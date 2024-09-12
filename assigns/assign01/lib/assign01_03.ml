open Assign01_02

let rec pow n k =
  if k = 0 then 1  
  else n * pow n (k - 1)  

let nth s i =
  let rec find_nth_element s i current_prime_index =
    let prime = nth_prime current_prime_index in
    let rec find_exponent s prime count =
      if s mod prime = 0 then find_exponent (s / prime) prime (count + 1)
      else count
    in
    let exponent = find_exponent s prime 0 in
    if current_prime_index = i then exponent
    else find_nth_element (s / (pow prime exponent)) i (current_prime_index + 1)
  in
  find_nth_element s i 0

