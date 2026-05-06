let rec lenr lst k =
  match lst with
    | [] -> k
    | x :: xs -> lenr xs k+1;;

let len lst = lenr lst 0;;

let println_int x = Printf.printf "%d\n" x;;
let () = ["1"; "2"; "3"; "4"; "5"] |> len |> println_int;; 