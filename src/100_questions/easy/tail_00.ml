let rec tail x =
  match x with
    | [] -> None
    | h :: [] -> Some h
    | h :: t -> tail t

let print_str_option s = 
  match s with
    | Some x -> print_endline x
    | None -> print_endline "Nothing!"

(*
   can also be ... = print_option @@ tail @@ ["1"; "2"; "3"; "4"; "5"]
*)
let () = ["1"; "2"; "3"; "4"; "5"] |> tail |> print_str_option;;