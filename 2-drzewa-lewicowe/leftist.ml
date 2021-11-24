(*
 * Leftist - Drzewa lewicowe
 * Code: Adam Greloch (438473)
 * Review: Wojciech Rzepliński (438709)
 *)

(** a' queue definiowane jako drzewo, gdzie węzły przetrzymują dodatkowo
    informację o odległości do najbliższego Leafa jako int *)
type 'a queue = Leaf | Node of 'a queue * 'a * 'a queue * int

exception Empty

let empty = Leaf;;

let is_empty q = (q = empty);;

(** zwraca liczbę wierzchołków potrzebnych do przejścia z danego wierzchołka do
    najbliższego liścia. *)
let s_val = function
    | Leaf -> -1
    | Node(_,_,_,d) -> d
;;

let rec join q1 q2 =
    match (q1, q2) with
    | (Leaf, _) -> q2
    | (_, Leaf) -> q1
    | (Node(_,k1,_,_), Node(_,k2,_,_)) when k2 < k1 -> join q2 q1
    | (Node(l1,k1,r1,_), _) ->
            let r1 = join r1 q2 in
            if s_val r1 > s_val l1 then
                Node(r1, k1, l1, s_val l1 + 1)
            else
                Node(l1, k1, r1, s_val r1 + 1)
;; 

let delete_min q =
    match q with
    | Leaf -> raise Empty
    | Node(l,k,r,_) -> (k, join l r)
;;

let add e =
    let single = Node(Leaf, e, Leaf, 0) in join single
;;

(* -------------------------------- Testy --------------------------------- *)
(*
exception WA;;

let test q l =
    try
      let (b, nq) = List.fold_left (fun a x -> 
        let (e, nq) = delete_min (snd a)
        in 
        if (compare x e != 0) then raise WA 
        else (true, nq)) 
        (true, q) l
      in
      b && (is_empty nq)
    with WA -> false
;;

let app h t = h::t

let q1 = empty |> add 3 |> add 5 |> add 10 |> add 2 |> add 2 |> add 7 |> add 22
let q2 = empty |> add 1 |> add 2 |> add 3 |> add 4 |> add 5

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [3; 5; 10; 2; 2; 7; 22]
let l2 = List.sort compare [1; 2; 3; 4; 5]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;
assert(try test empty l1 with Empty -> true);;

let rec add_lots f a l iter =
  if iter = 0 then a
  else
    add_lots f (List.fold_left (fun q x -> f x q) a l) l (iter - 1)

let q5 = add_lots add empty [1] 50000
let q6 = add_lots add q5 [2] 50000
let q7 = add_lots add empty [1; 2; 4; 5; 8] 20000
let q8 = join (join q5 q5) (join q6 q6)
let q9 = join (join q5 q6) (join q7 q8)

let l5a = add_lots app [] [1] 50000
let l5b = 1::l5a
let l5c = List.tl(l5a)
let l6 = l5a @ (add_lots app [] [2] 50000)
let l7 = List.sort compare (add_lots app [] [1; 2; 4; 5; 8] 20000)
let l8 = List.sort compare (l5a @ (l5a @ (l6 @ l6)))
let l9 = List.sort compare (l5a @ l6 @ l7 @ l8);;

assert(test q5 l5a);;
assert(try test q5 l5b with Empty -> true);;
assert(not(test q5 l5c));;
assert(test q6 l6);;
assert(test q7 l7);;
assert(test q8 l8);;
assert(test q9 l9);;
*)
