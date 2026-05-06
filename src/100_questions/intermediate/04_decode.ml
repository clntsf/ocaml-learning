type 'a rle =
  | One of 'a
  | Many of (int * 'a)

let rec decode lst =
  match lst with
    | [] -> []
    | One x :: xs
    | Many (1, x) :: xs ->
        x::decode xs
    | Many (k, x) :: xs -> x::decode (Many(k-1, x)::xs)