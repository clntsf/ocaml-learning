
let rec last_two = function
  | x :: y :: [] -> Some (x, y)
  | x :: t -> last_two t
  | _ -> None
;;

let print_pair_option = function
  | Some (x, y) -> print_endline ("(" ^ x ^ ", " ^ y ^ ")")
  | None -> print_endline "Nothing!";;

(*
   can also be ... = print_pair_option @@ tail @@ ["1"; "2"; "3"; "4"; "5"]
*)
let () = ["1"; "2"; "3"; "4"; "5"] |> last_two |> print_pair_option;;