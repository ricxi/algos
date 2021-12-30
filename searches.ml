let rec insert (x: int) (xs: int list) : int list =
  match xs with
  | [] -> [x]
  | h::t -> if x < h then x :: xs else h :: (insert x t);;

let insertion_sort (xs: int list) : int list =
  let rec aux (sorted: int list) (unsorted: int list) : int list =
    match unsorted with 
    | [] -> sorted
    | h::t -> aux (insert h sorted) t
  in aux [] xs;;
