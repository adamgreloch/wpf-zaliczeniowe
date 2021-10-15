(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(* Adam Greloch                                 *)
(************************************************)

open Float

type wartosc = float * float * int;;

(*
    type wartosc:

    (min, max, -1, 0, 1, 2)
    trzecia wartosc ustala znak 0, jeśli któreś jest zawarte
    -1: zawarte -0.0
     0: zawarte  0.0
    +1: zawarte +0.0
     2: brak zera
 *)

(* pomocnicze *)
let sgn n =
    match n with
    | 0.-> 0.
    | _ -> (n /. abs(n));
;;

(* konstruktory *)

let wartosc_dokladnosc x p =
    (* Input: p>0 *)
    if p > 0. then
        if ((x *. (1. -. p))*.(x *. (1. +. p)) < 0.) then
            (x *. (1. -. p), x *. (1. +. p), 0)
        else
            if (classify_float (x -. p) = FP_subnormal && x +. p > 0.) then
                (x *. (1. -. p), x *. (1. +. p), 1)
            else
                if (x -. p < 0. && classify_float (x +. p) = FP_subnormal) then
                    (x *. (1. -. p), x *. (1. +. p), -1)
                else
                    (x *. (1. -. p), x *. (1. +. p), 2)
    else
        failwith "Podano niewłaściwe p. Czy na pewno było dodatnie?"
;;

let wartosc_od_do x y =
    let czy_zero w = 
        match classify_float w with
        | FP_zero | FP_subnormal-> true
        | _ -> false
    in
    if x <= y then
        if (x *. y < 0.) then
            (x, y, 0)
        else
            if (czy_zero x && y > 0.) then
                (x, y, 1)
            else
                if (x < 0. && czy_zero y) then
                    (x, y, -1)
                else
                    (x, y, 2)
    else
        failwith "Ten przedział nie ma sensu"
;;

let wartosc_dokladna x = 
    match x with
    | 0. -> (x, x, 0)
    | _ -> (x, x, 2)
;;

(* selektory *)

let pokaz (a, b, c) = (a, b, c);;

let in_wartosc (min, max, _) x =
    if (min <= x && x <= max) then 
        true
    else
        false
;;

(* TODO: sprawdzić, czy na pewno trzeba brać tu pod uwagę nan *)
let min_wartosc (min, _, _) =
    match classify_float min with
    | FP_nan | FP_infinite -> neg_infinity
    | _ -> min
;;

let max_wartosc (_, max, _) =
    match classify_float max with
    | FP_nan | FP_infinite -> infinity
    | _ -> max
;;

let sr_wartosc (min, max, _) =
    if (min = neg_infinity && max = infinity) then
        nan
    else
        (min +. max) /. 2.
;;

let plus (x_min, x_max, x_zero) (y_min, y_max, y_zero) =
    let res_min = x_min +. y_min and res_max = x_max +. y_max
    in
    match (x_zero, y_zero) with
    | (0, _) | (_, 0)       -> (res_min, res_max, 0)
    | (1, -1) | (-1, 1)     -> (res_min, res_max, 0)
    | (1, 1)                -> (res_min, res_max, 1)
    | (1, 2) | (2, 1)       -> (res_min, res_max, 1)
    | (-1, -1)              -> (res_min, res_max, -1)
    | (-1, 2) | (2, -1)     -> (res_min, res_max, -1)
    | (_,_)                 -> (res_min, res_max, 2)
;;

let minus (x_min, x_max, x_zero) (y_min, y_max, y_zero) =
    if (y_min = 0. && y_max = 0.) then
        (x_min, x_max, x_zero)
    else
        let res_min = x_min -. y_max and res_max = x_max -. y_min
        in
        match (x_zero, y_zero) with
        | (-1, 0)               -> (res_min, res_max, 0)
        | (1, 0)                -> (res_min, res_max, 0)
        | (_, _)                -> (res_min, res_max, x_zero)
;;

let razy (x_min, x_max, x_zero) (y_min, y_max, y_zero) =
    if ((x_min = 0. && x_max = 0.) || (y_min = 0. && y_max = 0.)) then
        (0., 0., 0)
    else
        let ff = (x_min *. y_min)
        and fs = (x_min *. y_max)
        and sf = (x_max *. y_min)
        and ss = (x_max *. y_max)
        in
            let min1 = min ff fs
            and min2 = min sf ss
            and max1 = max ff fs
            and max2 = max sf ss
            in
            let res_min = min min1 min2 and res_max = max max1 max2
            in
            match (x_zero, y_zero) with
            | (0, _) | (_, 0)       -> (res_min, res_max, 0)
            | (1, 1)                -> (res_min, res_max, 1)
            | (-1, -1)              -> (res_min, res_max, 1)
            | (1, 2) | (2, 1)       -> (res_min, res_max, 1)
            | (1, -1) | (-1, 1)     -> (res_min, res_max, -1)
            | (-1, 2) | (2, -1)     -> (res_min, res_max, -1)
            | (_,_)                 -> (res_min, res_max, 2)
;;

let podzielic (x_min, x_max, x_zero) (y_min, y_max, y_zero) =
    match (x_zero, y_zero) with
    | (_, 0)  -> (nan, nan, -2)
    | (_, -1) -> 
            (* dzielenie przez (-1,-0.). Są następujące możliwości:
                1. (-1,+1,_) / (-1,-0.)   -> (-inf, +inf, 0)
                2. (-2,-1,_)             -> (1, +inf, 2)
                3. ( 1, 2,_)             -> (-inf, -1, 2) *)
            if (sgn x_min *. sgn x_max < 0.) then (neg_infinity, infinity, 0) else
            if (sgn x_min < 0. && sgn x_max < 0.) then (x_min /. y_min, infinity, 2) else
            if (sgn x_min > 0. && sgn x_max > 0.) then (neg_infinity, x_max /.  y_min, 2) else
                (0., 0., 0)
    | (_, 1)  ->
            (* dzielenie przez (0.,1.). Są następujące możliwości:
                1. (-1,+1,_) / (0.,1.)   -> (-inf, +inf, 0)
                2. (-2,-1,_)              -> (-inf, -1, 2)
                3. ( 1, 2,_)              -> (1, +inf, 2) *)

            if (sgn x_min *. sgn x_max < 0.) then (neg_infinity, infinity, 0) else
            if (sgn x_min < 0. && sgn x_max < 0.) then (neg_infinity, x_max /. y_max, 2) else
            if (sgn x_min > 0. && sgn x_max > 0.) then ( x_min /.  y_max, infinity, 2) else
                (0., 0., 0)
    | (_, _) ->
            let ff = (x_min /. y_min)
            and fs = (x_min /. y_max)
            and sf = (x_max /. y_min)
            and ss = (x_max /. y_max)
            in
                let min1 = min ff fs
                and min2 = min sf ss
                and max1 = max ff fs
                and max2 = max sf ss
                in
                (min min1 min2, max max1 max2, x_zero)
;;
