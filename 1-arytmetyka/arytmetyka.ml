(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(* Adam Greloch                                 *)
(************************************************)

open Float

type wartosc = float * float * float * float * int;;

(*
    type wartosc:

    (min1, max1, min2, max2, {-1, 0, 1, 2})
    
    Domyślnie liczby są zapisywane jako przedział (min1, max1) ale w przypadku
    dzielenia może zdarzyć się, że wynikiem będzie suma przedziałów, a na to
    wszystkie procedury również muszą być przygotowane.

    Integer na końcu ustala status zera:
    -2: nieoznaczona liczba
    -1: zawarte -0.0
     0: zawarte  0.0
    +1: zawarte +0.0
     2: brak zera

 *)

let pokaz (a, b, c, d, e) = (a, b, c, d, e);;

(* konstruktory *)

let wartosc_dokladnosc x p =
    (* Input: p>0 *)
    let sgn n =
        match n with
        | 0. -> 0.
        | _  -> (n /. abs(n))
    and a = x *. (1. -. 0.01 *. p)
    and b = x *. (1. +. 0.01 *. p)
    in
    if p >= 0. then
        if (sgn a *. sgn b < 0.) then
            (min a b, max a b, nan, nan, 0)
        else if (sgn a = 0. && sgn b > 0.) then
            (a, b, nan, nan, 1)
        else if (sgn a < 0. && sgn b = 0.) then
            (a, b, nan, nan, -1)
        else
            (min a b, max a b, nan, nan, 2)
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
            (x, y, nan, nan, 0)
        else
            if (czy_zero x && y > 0.) then
                (x, y, nan, nan, 1)
            else
                if (x < 0. && czy_zero y) then
                    (x, y, nan, nan, -1)
                else
                    (x, y, nan, nan, 2)
    else
        failwith "Ten przedział nie ma sensu"
;;

let wartosc_dokladna x = 
    match x with
    | 0. -> (x, x, nan, nan, 0)
    | _ -> (x, x, nan, nan, 2)
;;

(* selektory *)

let in_wartosc (min1, max1, min2, max2, zero) x =
    if (zero = 0 && x = 0.) then true
    else
    if ((min1 <= x && x <= max1) || (min2 <= x && x <= max2)) then 
        true
    else
        false
;;

(* TODO: sprawdzić, czy na pewno trzeba brać tu pod uwagę nan *)
let min_wartosc (min, _, _, _, _) =
    match classify_float min with
    | FP_infinite -> neg_infinity
    | _ -> min
;;

let max_wartosc (_, max1, _, max2, _) =
    match classify_float max2 with
    | FP_nan -> max1
    | _ -> max2
;;

let sr_wartosc (min1, max1, _, max2, _) =
    if (not (classify_float max2 = FP_nan)) then
        nan
    else
    if (classify_float min1 = FP_infinite && classify_float max1 = FP_infinite) then
        nan
    else
        (min1 +. max1) /. 2.
;;

let plus (x_min1, x_max1, x_min2, x_max2, x_zero) (y_min1, y_max1, y_min2,
y_max2, y_zero) =
    let sum zero = 
        (* pomocnicza sumująca ew. nachodzące się przedziały i zwracająca
           wartosc *)
        if (classify_float x_min2 = FP_nan && classify_float y_min2 =
            FP_nan) then
            (x_min1 +. y_min1, x_max1 +. y_max1, nan, nan, zero)
        else if (classify_float x_min2 = FP_nan) then
            let res_min1 = y_min1
            and res_max1 = x_min1 +. (max y_min1 y_max2)
            and res_min2 = x_max1 +. (min y_min2 y_max2)
            and res_max2 = y_max2
            in
            if (res_min2 <= res_max1) then
                (res_min1, res_max2, nan, nan, zero)
            else
                (res_min1, res_max1, res_min2, res_max2, zero)
        else if (classify_float y_min2 = FP_nan) then
            let res_min1 = x_min1
            and res_max1 = y_min1 +. (max x_min2 x_max1)
            and res_min2 = y_max1 +. (min x_max1 x_min2)
            and res_max2 = x_max2
            in
            if (res_min2 <= res_max1) then
                (res_min1, res_max2, nan, nan, zero)
            else
                (res_min1, res_max1, res_min2, res_max2, zero)
        else
            let res_min1 = x_min1 +. y_min1 and res_max1 = x_max1 +. y_max1
            and res_min2 = x_min2 +. y_min2 and res_max2 = x_max2 +. y_max2
            in
            if (res_min2 <= res_max1) then
                (res_min1, res_max2, nan, nan, zero)
            else
                (res_min1, res_max1, res_min2, res_max2, zero)
    in
    match (x_zero, y_zero) with
    | (0, _) | (_, 0)       -> sum 0
    | (1, -1) | (-1, 1)     -> sum 0
    | (1, 1)                -> sum 1
    | (1, 2) | (2, 1)       -> sum 1
    | (-1, -1)              -> sum (-1)
    | (-1, 2) | (2, -1)     -> sum (-1)
    | (_,_)                 -> sum 2
;;

let minus (x_min1, x_max1, x_min2, x_max2, x_zero) (y_min1, y_max1, y_min2,
y_max2, y_zero) =
    let res_min1 = x_min1 -. y_max1 and res_max1 = x_max1 -. y_min1
    and res_min2 = x_min2 -. y_max2 and res_max2 = x_max2 -. y_min2
    in
    let sum zero = 
        (* pomocnicza sumująca ew. nachodzące się przedziały i zwracająca
           wartosc *)
        if (res_min2 < res_max1) then
            (res_min1, res_max2, nan, nan, zero)
        else
            (res_min1, res_max1, res_min2, res_max2, zero)
    in
    if (y_min1 = 0. && y_max1 = 0.) then
        if (y_min2 = 0. && y_max2 = 0.) then
            (x_min1, x_max1, x_min2, x_max2, x_zero)
        else
            (x_min1, x_max1, res_min2, res_max2, x_zero)
    else
        match (x_zero, y_zero) with
        | (-1, 0)               -> sum 0
        | (1, 0)                -> sum 0
        | (_, _)                -> sum x_zero
;;

let razy (x_min1, x_max1, x_min2, x_max2, x_zero) (y_min1, y_max1, y_min2,
y_max2, y_zero) =
    if ((x_min1 = 0. && x_max1 = 0.) || (y_min1 = 0. && y_max1 = 0.)) then
        (0., 0., nan, nan, 0)
    else
        let minimize a_min (b1, b2, b3, b4) =
            if (classify_float b3 = FP_nan && classify_float b4 = FP_nan) then
                min (a_min *. b1) (a_min *. b2)
            else
                min (min (a_min *. b1) (a_min *. b2)) (min (a_min *. b3) (a_min *. b4))
        and maximize a_max (b1, b2, b3, b4) =
            if (classify_float b3 = FP_nan && classify_float b4 = FP_nan) then
                max (a_max *. b1) (a_max *. b2)
            else
                max (max (a_max *. b1) (a_max *. b2)) (max (a_max *. b3) (a_max *. b4))
        in
        let sum zero = 
            let y = (y_min1, y_max1, y_min2, y_max2)
            in
            let res_min1 = min (minimize x_min1 y) (minimize x_max1 y)
            and res_max1 = max (maximize x_min1 y) (maximize x_max1 y)
            and res_min2 = min (minimize x_min2 y) (minimize x_max2 y)
            and res_max2 = max (maximize x_min2 y) (maximize x_max2 y)
            in
            (* pomocnicza sumująca ew. nachodzące się przedziały i zwracająca
               wartosc *)
            if (res_min2 < res_max1) then
                (res_min1, res_max2, nan, nan, zero)
            else
                (res_min1, res_max1, res_min2, res_max2, zero)
        in
        match (x_zero, y_zero) with
        | (0, _) | (_, 0)       -> sum 0
        | (1, 1)                -> sum 1
        | (-1, -1)              -> sum 1
        | (1, 2) | (2, 1)       -> sum 1
        | (1, -1) | (-1, 1)     -> sum (-1)
        | (-1, 2) | (2, -1)     -> sum (-1)
        | (_,_)                 -> sum 2
;;

let podzielic (x_min1, x_max1, x_min2, x_max2, x_zero) (y_min1, y_max1, y_min2,
y_max2, y_zero) =
    let sgn n =
        match n with
        | 0. -> 0.
        | _  -> (n /. abs(n))
        (*
    and minimize a_min (b1, b2, b3, b4) =
        min (min (a_min /. b1) (a_min /. b2)) (min (a_min /. b3) (a_min /. b4))
    and maximize a_max (b1, b2, b3, b4) =
        max (max (a_max /. b1) (a_max /. b2)) (max (a_max /. b3) (a_max /. b4))
        *)
    in
    if (classify_float x_min2 = FP_nan && classify_float y_min2 = FP_nan) then
    if (y_min1 = 0. && y_max1 = 0.) then
        (nan, nan, nan, nan, -2)
    else
    match (x_zero, y_zero) with
    | (_, 0)  ->
            (* dzielenie przez liczbę zawierającą obustronne 0.0.
                1. (-4,+8, _, _, 0) / (-2,+2,0)  -> (neg_infinity, -2, 2, infinity, 2)
                2. (-4,-2, _, _, 2) / (-2,+2,0)  -> (neg_infinity, -1, 1, infinity, 2)
                3. (2, 4, _, _, 2) / (-2,+2,0)  -> (neg_infinity, -1, 1, infinity, 2) *)
            if (sgn x_min1 *. sgn x_max1 < 0.) then
                (neg_infinity, x_min1 /. y_max1, x_min1 /. y_min1, infinity, 2)
            else if (sgn x_min1 < 0. && sgn x_max1 < 0.) then
                (neg_infinity, x_max1 /. y_max1, x_max1 /. y_min1, infinity, 2)
            else if (sgn x_min1 > 0. && sgn x_max1 > 0.) then
                (neg_infinity, x_min1 /. y_min1, x_max1 /. y_max1, infinity, 2)
            else if (sgn x_min1 < 0. && x_max1 = 0.) then
                (* TODO: "let o" z testów *)
                (0., infinity, nan, nan, 0)
            else if (sgn x_min1 = 0. && x_max1 > 0.) then
                (neg_infinity, 0., nan, nan, 0)
            else
                (* TODO: upewnić się że tak będzie: *)
                (nan, nan, nan, nan, 2)
    | (_, -1) -> 
            (* dzielenie przez np. (-1,-0.). Są następujące możliwości:
                1. (-1,+1,0) / (-1,-0.)   -> (-inf, +inf, 0)
                2. (-2,-1,2)              -> (1, +inf, 2)
                3. ( 1, 2,2)              -> (-inf, -1, 2) *)
            if (sgn x_min1 *. sgn x_max1 < 0.) then
                (neg_infinity, infinity, nan, nan, 0)
            else if (sgn x_min1 < 0. && sgn x_max1 < 0.) then
                (x_min1 /. y_min1, infinity, nan, nan, 2)
            else if (sgn x_min1 > 0. && sgn x_max1 > 0.) then
                (neg_infinity, x_max1 /. y_min1, nan, nan, 2)
            else
                (0., 0., nan, nan, 0)
    | (_, 1)  ->
            (* dzielenie przez np. (0.,1.). Są następujące możliwości:
                1. (-1,+1,_) / (0.,1.)    -> (-inf, +inf, 0)
                2. (-2,-1,_)              -> (-inf, -1, 2)
                3. ( 1, 2,_)              -> (1, +inf, 2) *)
            if (sgn x_min1 *. sgn x_max1 < 0.) then
                (neg_infinity, infinity, nan, nan, 0)
            else if (sgn x_min1 < 0. && sgn x_max1 < 0.) then
                (neg_infinity, x_max1 /. y_max1, nan, nan, 2)
            else if (sgn x_min1 > 0. && sgn x_max1 > 0.) then
                (x_min1 /. y_max1, infinity, nan, nan, 2)
            else
                (0., 0., nan, nan, 0)
    | (_, _) ->
            let ff = (x_min1 /. y_min1)
            and fs = (x_min1 /. y_max1)
            and sf = (x_max1 /. y_min1)
            and ss = (x_max1 /. y_max1)
            in
                let min1 = min ff fs
                and min2 = min sf ss
                and max1 = max ff fs
                and max2 = max sf ss
                in
                (min min1 min2, max max1 max2, nan, nan, x_zero)
    else
        (* TODO: ogarnac mnozenie dla dwuprzedzialowych liczb jesli to w ogole
           mozliwe? *)
        (nan, nan, nan, nan, 0) (* placeholder *)
;;
