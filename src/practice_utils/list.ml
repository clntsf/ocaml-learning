(* Writing some sample list utils for practice *)

let rec map f = function
  | [] -> []
  | x::xs -> f x :: xs
;;

let len lst =
  let rec aux k = function
  | [] -> k
  | x::xs -> aux (k+1) xs 
in aux 0;;

let rec foldl unit f = function
  | [] -> unit
  | x::xs -> foldl (f unit x) f xs
;;

let rec foldr unit f = function
  | [] -> unit
  | x::xs -> f x (foldr unit f xs)
;;


(* we can use short-circuiting in the 3 below to remove duplicate arms *)
let rec member k = function
  | [] -> false
  | x::xs -> x = k && member k xs
;;

let rec all f = function
  | [] -> true
  | x::xs -> f x && all f xs
;;

let rec any f = function
  | [] -> false
  | x::xs -> f x || any f xs
;;

let rec drop k = function
  | [] -> []
  | lst when k = 0 -> lst
  | x::xs -> drop (k-1) xs
;;

let rec take k = function
  | [] -> []
  | lst when k = 0 -> []
  | x::xs -> x::take (k-1) xs
;;

(* 
   just for instructional purposes for me – I will probably make
   an operators.ml in this folder to teach myself about interesting
   infix operations (alternation, composition etc)

   the below is just |>
*)
let ( *|> ) x f = f x;;

let slice a b lst = lst *|> (drop a) *|> take (b - a + 1);;