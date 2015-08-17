(** Twisted ways to compute lengths of lists *)

let length l =
  let f g n l =
    match l with
    | [] -> n
    | _::tl -> g (n+1) tl
  in
  let rec h n l = f h n l
  in
  h 0 l

let _ = length [2;3;4;4]

