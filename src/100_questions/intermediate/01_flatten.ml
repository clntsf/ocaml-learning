type 'a node =
  | One of 'a
  | Many of 'a node list

(* List.concat exists *)
let rec concat b = function
  | [] -> b
  | x :: xs -> x :: concat b xs
;;

let rec flatten = function
  | [] -> []
  | One x::xs -> One x :: flatten xs
  | Many sl :: xs -> concat (flatten sl) (flatten xs);;

(*
utop # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
- : string node list = [One "a"; One "b"; One "c"; One "d"; One "e"]
*)