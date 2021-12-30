let rec insert (x: int) (xs: int list) : int list =
  match xs with
  | [] -> [x]
  | h::t -> if x < h then x :: xs else h :: (insert x t);;

  