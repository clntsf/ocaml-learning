type op = 
  | Value of int
  | Unary of (int -> int) * op
  | Binary of (int -> int -> int) * op * op

let rec calc = function
  | Value x -> x
  | Unary (fn, x) -> fn (calc x)
  | Binary (fn, x, y) -> fn (calc x) (calc y)
;;

let evalop = function
| "+" -> (+)
| "-" -> (-)
| "*" -> ( * )
| "/" -> (/)
| _ -> failwith "bad op"
;;

let isop = function
  | "+" | "-" | "*" | "/" -> true
  | _ -> false

let isdigit x =
  match int_of_string_opt x with
    | None -> false
    | Some x -> true

(*
  I will come back to this later and maybe think it's bad,
  but it works! teacher claude has suggested learning ocamllex and Menhir
  so my days of manually hacking grammars may end soonish
*)
let parse_postfix toks =
  let rec aux stack toks =
    match stack, toks with
      | x::_, [] -> calc x
      | a::b::stk, t::ts when isop t-> aux (Binary (evalop t, a, b)::stk) ts
      | stk, t::ts when isdigit t -> aux (Value (int_of_string t)::stk) ts
      | _ -> failwith "Invalid Input"
in aux [] toks

let println_int x = Printf.printf "%d\n" x;;

let () = parse_postfix ["3"; "5"; "+"; "8"; "*"] |> println_int;;
