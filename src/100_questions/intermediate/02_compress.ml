
(*
  note from claude:
  it's better to use y :: xs than x :: xs here,
  since we've already consed it implicitly in the y :: xs match,
  so it won't require a new allocation like x :: xs would
*)
let rec compress = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: xs when x = y -> compress (y :: xs)
  | x :: y :: xs -> x :: compress (y :: xs)