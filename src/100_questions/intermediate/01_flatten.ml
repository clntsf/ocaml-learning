type 'a node =
  | One of 'a
  | Many of 'a node list

let rec concat a b =
  match a with
    | [] -> b
    | x :: xs -> x :: concat xs b;;

let rec flatten lst =
  match lst with
    | [] -> []
    | One x :: xs -> One x :: flatten xs
    | Many sl :: xs -> concat (flatten sl) (flatten xs);;

(*
utop # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
- : string node list = [One "a"; One "b"; One "c"; One "d"; One "e"]
*)