let sqrt n =
  let rec aux k =
    if k * k >= n then k
    else aux (k + 1)
  in aux 0
