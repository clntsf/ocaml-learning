let rec last lst =
  match lst with
    | [] -> None
    | x :: [] -> Some x
    | x :: xs -> last xs;;

let print_str_option s = 
  match s with
    | Some x -> print_endline x
    | None -> print_endline "Nothing!";;

(*
   can also be ... = print_str_option @@ tail @@ ["1"; "2"; "3"; "4"; "5"]
*)
let () = ["1"; "2"; "3"; "4"; "5"] |> last |> print_str_option;;