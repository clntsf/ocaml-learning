let len =
  let rec aux k = function
    | [] -> k
    | x :: xs -> aux (k+1) xs
in aux 0;;

let println_int x = Printf.printf "%d\n" x;;
let () = ["1"; "2"; "3"; "4"; "5"] |> len |> println_int;; 