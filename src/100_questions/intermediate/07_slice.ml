let slice lst a b =
  let rec dropfirst k lst =
    match k, lst with
      | 0, lst -> lst
      | _, [] -> []
      | n, x::xs -> dropfirst (n - 1) xs in
  let rec take k lst = 
    match k, lst with
      | 0, _ -> []
      | _, [] -> []
      | n, x::xs -> x::(take  (n - 1) xs) in
lst |> dropfirst a |> take (b - a + 1);;