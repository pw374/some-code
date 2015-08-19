(** Twisted ways to compute lengths of lists *)
(* © 2015 Philippe Wang <philippe.wang@gmail.com> *)

module type ListLength =
sig val length : 'a list -> int end

module NaiveNormalWay : ListLength =
struct
  let rec length = function [] -> 0 | _ :: tl -> 1 + length tl

  let test = length [2;3;4;4]
end

module TailRecWay : ListLength =
struct
  let length l =
    let rec f n = function [] -> n | _ :: tl -> f (succ n) tl
    in
    f 0 l

  let test = length [2;3;4;4]
end

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

(** no keyword 'rec', using objects. *)
module D : ListLength =
struct
  let length l =
    let o =
      object(self)
        method f a = function
          | [] -> a
          | _ :: tl -> self#f (a+1) tl
      end
    in
    o#f 0 l

  let test = length [2;3;4;4]
end


(** impure with a reference, to avoid rec. *)
module E : ListLength =
struct
  let length l =
    let f = ref (fun _ -> failwith "failure") in
    let g n = function
      | [] -> n
      | _ :: tl -> !f (n+1) tl
    in
    f := g;
    g 0 l

  let test = length [2;3;4;4]
end


(** impure and basic with while loop. *)
module F : ListLength =
struct
  let length l =
    let r = ref 0 in
    let l = ref l in
    while !l <> [] do incr r; l := List.tl !l done;
    !r    

  let test = length [2;3;4;4]
end

(** Short. *)
module G : ListLength =
struct
  let length l =
    let r = ref 0 in
    List.iter (fun _ -> incr r) l;
    !r

  let test = length [2;3;4;4]
end


(** Stupid (and may use a lot of memory). *)
module H : ListLength =
struct
  let length l = Array.length (Array.of_list l)

  let test = length [2;3;4;4]
end


(** Twisted version of F. *)
module FT : ListLength =
struct
  let length l =
    let r = ref (0, l) in
    while snd !r <> [] do
      r := succ(fst !r), List.tl (snd !r);
    done;
    fst !r

  let test = length [2;3;4;4]
end

(** The other twisted version of F. *)
module FTT : ListLength =
struct
  let length l =
    let r = ref 0, ref l in
    while !(snd r) <> [] do
      fst r := succ !(fst r);
      snd r := List.tl !(snd r)
    done;
    !(fst r)

  let test = length [2;3;4;4]
end

