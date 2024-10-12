module ListSet = struct
  type t = int list

  let empty = []

  let singleton x = [x]

  let rec mem x lst =
    match lst with
    | [] -> false
    | y :: ys ->
        if x = y then true
        else if x > y then mem x ys
        else false

  let card lst = List.length lst

  let rec union xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | xh :: xt, yh :: yt ->
        if xh = yh then xh :: union xt yt
        else if xh < yh then xh :: union xt ys
        else yh :: union xs yt
end

type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module FuncSet = struct
  type t = set_info

  let empty = {
    ind = (fun _ -> false);
    mn = 1;
    mx = 0;
  }

  let singleton x = {
    ind = (fun y -> y = x);
    mn = x;
    mx = x;
  }

  let mem x s = s.ind x

  let card s =
    if s.mn > s.mx then 0
    else
      let rec count n acc =
        if n > s.mx then acc
        else if s.ind n then count (n + 1) (acc + 1)
        else count (n + 1) acc
      in
      count s.mn 0

  let union s1 s2 =
    let ind x = s1.ind x || s2.ind x in
    let mn, mx =
      if s1.mn > s1.mx then
        if s2.mn > s2.mx then 1, 0  
        else s2.mn, s2.mx
      else if s2.mn > s2.mx then s1.mn, s1.mx
      else min s1.mn s2.mn, max s1.mx s2.mx
    in
    { ind; mn; mx }
end
