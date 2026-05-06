let rec concat a b =
  match a with
    | [] -> b
    | x :: xs -> x :: concat xs b;;

let rec packr m lst = 
match m, lst with
  | m, [] -> [m]
  | m::ms, x::xs ->
      let lm = m::ms in
      if x = m                    (* equality comparison is = not like haskell *)
        then packr (x::lm) (xs)
        else lm::(packr [] (x::xs))
  | [], x::xs -> packr [x] xs

let pack = packr []