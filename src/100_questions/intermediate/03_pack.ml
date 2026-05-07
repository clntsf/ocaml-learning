let rec concat b = function
  | [] -> b
  | x :: xs -> x :: concat b xs
;;

let pack lst =
  let rec packr ch ct = function
    | [] -> [ch :: ct]
    | x::xs when x = ch -> packr x (ch::ct) xs
    | x :: xs -> (ch::ct) :: packr x [] xs
in
match lst with
  | [] -> []
  | x :: xs -> packr x [] xs
;;