type 'a test = 
  | TestCase of 'a
  | TestList of 'a test list

let rec fold_left op acc test_suite = 
  match test_suite with
  | TestCase x -> op acc x
  | TestList tests -> List.fold_left (fun acc t -> fold_left op acc t) acc tests

let test_suite = 
  TestList [
    TestCase 1; 
    TestList [TestCase 2; TestCase 3]; 
    TestCase 4
  ]

let op acc x = x :: acc




