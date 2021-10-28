(*                *******************************************               *)
(*                Drzewa Lewicowe                                           *)
(*                Kod: Adam Greloch (438473)                                *)
(*                Review: Marcin Żołek (438836)                             *)
(*                *******************************************               *)

(** a' queue definiowane jako drzewo, gdzie węzły przetrzymują dodatkowo
    informację o odległości do najbliższego Leafa jako int *)
type 'a queue = Leaf | Node of 'a queue * 'a * 'a queue * int

exception Empty

let empty = Leaf;;

let is_empty q = (q = empty);;

(** zwraca s-value danego korzenia *)
let s_val = function
    | Leaf -> -1
    | Node(_,_,_,d) -> d
;;

let rec join q1 q2 =
    match (q1, q2) with
    | (Leaf, _) -> q2
    | (_, Leaf) -> q1
    | (Node(_,k1,_,_), Node(_,k2,_,_)) when k2 < k1 -> join q2 q1
    | (Node(l1,k1,r1,_), Node(_,k2,_,_)) ->
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
