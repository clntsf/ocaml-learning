let rec revr a b =
  match a with
    | [] -> b
    | x :: xs -> revr xs (x :: b);;

let rev lst = revr lst [];;

let rec str_list_r lst =
  match lst with
    | [] -> ""
    | x :: [] -> x
    | x :: xs -> x ^ ", " ^ (str_list_r xs);;

let () = ["1"; "2"; "3"; "4"; "5"] |> rev |> str_list_r |> print_endline;; 