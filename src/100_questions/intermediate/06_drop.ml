let drop lst k = 
  let rec aux j k lst = 
    match j, k, lst with
    | _, _, [] -> []
    | 1, k, x::xs -> aux k k xs
    | n, k, x::xs -> x::aux (n-1) k (xs)
in aux k k lst;;