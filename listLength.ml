(** Twisted ways to compute lengths of lists *)
(* © 2015 Philippe Wang <philippe.wang@gmail.com> *)

module type ListLength =
sig val length : 'a list -> int end 

(** deported and abstract recursion, tailrec *)
module A : ListLength =
struct
  let length l =
    let f g n l =
      match l with
      | [] -> n
      | _::tl -> g (n+1) tl
    in
    let rec h n l = f h n l
    in
    h 0 l

  let test = length [2;3;4;4]
end

(** deported recursion, tailrec, without using the keyword 'rec' *)
module B : ListLength =
struct
  type 'a t = A of int * ('a t -> 'a list -> int)
  let f (A (n, g)) l =
    match l with
    | [] -> n
    | _ :: tl ->
      g (A(n+1, g)) tl
  let length l = f (A(0, f)) l
  (* 'rec' is actually handled by 'type', which implies 'rec' *)

  let test = length [2;3;4;4]
end

(** deported recursion, tailrec, without using the keyword 'rec',
    and without defining a type. *)
module C : ListLength =
struct
  let f (`A (n, g)) l =
    match l with
    | [] -> n
    | _ :: tl ->
      g (`A(n+1, g)) tl
  let length l = f (`A(0, f)) l
  (* same as module B, but using polymorphic variants *)

  let test = length [2;3;4;4]
end
