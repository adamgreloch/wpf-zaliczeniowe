(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(* Adam Greloch                                 *)
(************************************************)

type wartosc = float * float;;

(* konstruktory *)

let wartosc_dokladnosc x p =
    (* Input: p>0 *)
    if p > 0. then
        (x *. (1. -. p), x *. (1. +. p))
    else
        failwith "Podano niewłaściwe p. Czy na pewno było dodatnie?"
;;

let wartosc_od_do x y =
    if x <= y then
        (x, y)
    else
        failwith "Ten przedział nie ma sensu"
;;

let wartosc_dokladna x = (x, x);;

(* selektory *)

let in_wartosc w x =
    if (snd w >= x) then 
        true
    else
        if (fst w <= x) then 
            true
        else
            false
;;

(* TODO: sprawdzić, czy na pewno trzeba brać tu pod uwagę nan *)
let min_wartosc w =
    match classify_float(fst w) with
    | FP_nan | FP_infinite -> neg_infinity
    | _ -> fst w
;;

let max_wartosc w =
    match classify_float(snd w) with
    | FP_nan | FP_infinite -> infinity
    | _ -> snd w
;;

let sr_wartosc w =
    let min_w = min_wartosc w and max_w = max_wartosc w in
    if (min_w = neg_infinity && max_w = infinity) then
        nan
    else
        (min_w +. max_w) /. 2.
;;

let plus x y =
    (fst x +. fst y, snd x +. snd y)
;;

let minus x y =
    (fst x -. snd y, snd x -. fst y)
;;

let razy x y =
    let ff = (fst x *. fst y)
    and fs = (fst x *. snd y)
    and sf = (snd x *. fst y)
    and ss = (snd x *. snd y)
    in
        let min1 = min ff fs
        and min2 = min sf ss
        and max1 = max ff fs
        and max2 = max sf ss
        in
        (min min1 min2, max max1 max2)
;;

