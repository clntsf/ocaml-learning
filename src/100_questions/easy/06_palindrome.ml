let rev =
  let rec helper aux = function
    | [] -> aux
    | x::xs -> helper (x::aux) xs
in helper [];;

(* apparently (=) works for lists: who knew! *)
let rec eq a b = 
  match a, b with
    | [], [] -> true
    | x::xs, y::ys ->  x=y && eq xs ys
    | _ -> false
;;

let palindrome x = (=) x (rev x)

let rec print_bool b =
  print_endline (if b then "true" else "false")
;;

let () = ["1"; "2"; "3"; "2"; "1"] |> palindrome |> print_bool;; 