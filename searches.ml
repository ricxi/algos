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
  
let len (xs: int list) : int =
  let rec aux (xs: int list) (acc : int) : int =
    match xs with 
    | [] -> acc
    | _::t -> aux t (acc+1)
  in aux xs 0;;

