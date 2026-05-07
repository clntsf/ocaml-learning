let rec at k lst =
  match k, lst with
    | 1, x :: _ -> Some x
    | _, [] -> None
    | n, x :: xs -> at (n-1) xs;;

let print_str_option = function
  | Some x -> print_endline x
  | None -> print_endline "Nothing!";;

(*
   can also be ... = print_str_option @@ tail @@ ["1"; "2"; "3"; "4"; "5"]
*)
let () = ["1"; "2"; "3"; "4"; "5"] |> (at 3) |> print_str_option;;