let rec compress lst = 
  match lst with
    | [] -> []
    | x :: [] -> x :: []
    | x :: y :: xs ->
        if x = y                    (* equality comparison is = not like haskell *)
          then compress (x::xs)
          else x :: compress (y::xs)