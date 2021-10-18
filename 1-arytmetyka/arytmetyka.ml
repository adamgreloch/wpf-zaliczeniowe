(*       **********************************************************         *)
(*              Zadanie o arytmetyce niedokładnych wartości.                *)
(*              Kod: Adam Greloch                                           *)
(*              Review:                                                     *)
(*       **********************************************************         *)

(* ----------------- Definicje typów i funkcje wewnętrzne ----------------- *)

open Float

(** Redefinicja operacji mnożenia, uwzględniająca dla uproszczenia
(neg_)infinity * 0.0 = 0.0. *)
let ( *. ) a b =
    if (a = 0. || b = 0.) then
        0.
    else
        a *. b
;;

(* TODO: Może być potrzebna redefinicja dodawania też *)

type przedzial =
    {
        min: float;
        max: float
    }

let pusty =
    {
        min = nan;
        max = nan
    }

(** 
    Podstawowy typ przechowywania niedokładnych wartości, składający się z
    dwóch przedziałów: `fst` i `snd`. Definiując dwa przedziały zapobiegamy
    utracie informacji podczas dzielenia, którego wynikiem może okazać się
    dopełnienie przedziału. Warto zauważyć, że wartość dwuprzedziałowa zawsze
    będzie tak naprawdę zbiorem R z wyłączeniem określonego przedziału <a,b>.
*)
type wartosc = 
    {
        fst: przedzial;
        snd: przedzial
    }

(** Sprawdza, czy dany x: wartość jest dwuprzedziałowy *)
let czy_dwa x =
    if (classify_float x.snd.min = FP_nan) then
        false
    else
        true
;;

(** Sprawdza, czy dany x: wartość jest pusty *)
let czy_pusty x =
    if (classify_float x.fst.min = FP_nan) then
        true
    else
        false
;;

(** Zwraca znak floata *)
let sgn n =
    match n with
    | 0. -> 0.
    | _  -> (n /. abs_float(n))
;;

(** Zwraca odwrotność wartości *)
let odwrotnosc x =
    if (czy_dwa x) then
        if (sgn x.snd.min *. sgn x.fst.max < 0.) then
            {fst = {min = 1. /. x.fst.max; max = 1. /. x.snd.min}; snd = pusty}
        else
            {fst = {min = 1. /. x.snd.min; max = 1. /. x.fst.max}; snd = pusty}
    else
        {fst = {min = neg_infinity; max = 1. /. x.fst.max}; snd = {min = 1. /.
        x.fst.min; max = infinity}}
;;

(** Zwraca przedział R z wykluczonym przedziałem x *)
let wyklucz x =
    {fst = {min = neg_infinity; max = x.fst.min};
     snd = {min = x.fst.max; max = infinity}}
;;
  
(** Przyjmuje dwuprzedziałową liczbę i zwraca złączony jeden, jeśli przedziały
się przecinają *)
let suma w =
    if (w.fst.max >= w.snd.min) then
        {fst = {min = w.fst.min; max = w.snd.max};
         snd = pusty}
    else
        w
;;

let pokaz x = (x.fst.min, x.fst.max, x.snd.min, x.snd.max);;

(* ----------------------------- Konstruktory ----------------------------- *)

let wartosc_dokladnosc x p =
    let a = x *. (1. -. 0.01 *. p)
    and b = x *. (1. +. 0.01 *. p)
    in
    {fst = {min = min a b; max = max a b}; snd = pusty}
;;


let wartosc_od_do x y =
    {fst = {min = x; max = y}; snd = pusty}
;;

let wartosc_dokladna x = 
    {fst = {min = x; max = x}; snd = pusty}
;;

(* ------------------------------ Selektory ------------------------------- *)

let in_wartosc w x =
    if (w.fst.min <= x && x <= w.fst.max || w.snd.min <= x && x <= w.snd.max)
    then
        true
    else
        false
;;

let min_wartosc w = w.fst.min;;

let max_wartosc w =
    if (czy_dwa w) then
        w.snd.max
    else
        w.fst.max
;;

let sr_wartosc w =
    if (czy_dwa w) then
        nan
    else
        (w.fst.min +. w.fst.max) /. 2.
;;

(* ------------------------------ Operatory ------------------------------- *)

let plus w z =
    match (czy_dwa w, czy_dwa z) with
    | (true, true) ->
            (* suma dwóch wartości dwuprzedziałowych daje R *)
            {fst = {min = neg_infinity; max = infinity}; snd = pusty}
    | (true, false) ->
            (* TODO: upewnić się *)
            suma {fst = {min = neg_infinity; max = w.fst.max +. z.fst.max};
             snd = {min = w.snd.min +. z.fst.min; max = infinity}}
    | (false, true) ->
            suma {fst = {min = neg_infinity; max = z.fst.max +. w.fst.max};
             snd = {min = z.snd.min +. w.fst.min; max = infinity}}
    | (false, false) ->
            {fst = {min = w.fst.min +. z.fst.min; max = w.fst.max +. z.fst.max};
             snd = pusty};
;;

let minus w z =
    match (czy_dwa w, czy_dwa z) with
    | (true, true) ->
            (* różnica dwóch wartości dwuprzedziałowych daje R *)
            {fst = {min = neg_infinity; max = infinity}; snd = pusty}
    | (true, false) ->
            (* TODO: upewnić się *)
            suma {fst = {min = neg_infinity; max = w.fst.max -. z.fst.max};
             snd = {min = w.snd.min -. z.fst.min; max = infinity}}
    | (false, true) ->
            (* TODO: sprawdzić czy na pewno. Jeśli na pewno to połączyć z
               (true, true) *)
            suma {fst = {min = neg_infinity; max = infinity}; snd = pusty}
    | (false, false) ->
            {fst = {min = w.fst.min -. z.fst.max; max = w.fst.max -. z.fst.min};
             snd = pusty}
;;

let razy w z =
    match (czy_dwa w, czy_dwa z) with
    | (true, true) ->
            (* iloczyn dwóch wartości dwuprzedziałowych daje R *)
            {fst = {min = neg_infinity; max = infinity}; snd = pusty}
    | (true, false) ->
            (* TODO: zrobić zaraz eksperyment i złączyć oba te patterny
               kilkoma minimum *)
            suma {fst = {min = neg_infinity *. abs(z.fst.max); max = w.fst.max *. z.fst.max};
             snd = {min = w.snd.min *. z.fst.min; max = infinity *.
             abs(z.fst.min)}}
    | (false, true) ->
            let a = w.fst.max *. z.fst.max
            and b = w.fst.min *. z.snd.min
            in
                suma {fst = {min = neg_infinity *. abs(w.fst.max); max = min a b};
                 snd = {min = max a b; max = infinity *.  abs(w.fst.min)}}
    | (false, false) ->
            let a_min = min (w.fst.min *. z.fst.min) (w.fst.min *. z.fst.max)
            and b_min = min (w.fst.max *. z.fst.min) (w.fst.max *. z.fst.max)
            and a_max = max (w.fst.min *. z.fst.min) (w.fst.min *. z.fst.max)
            and b_max = max (w.fst.max *. z.fst.min) (w.fst.max *. z.fst.max)
            in
                {fst = {min = min a_min b_min; max = max a_max b_max};
                 snd = pusty}
;;

let podzielic w z =
    if (z.fst.min = 0. && z.fst.max = 0.) then
        {fst = pusty; snd = pusty}
    else
        match (czy_dwa w, czy_dwa z) with
        | (true, true) ->
                (* iloczyn dwóch wartości dwuprzedziałowych daje R *)
                {fst = {min = neg_infinity; max = infinity}; snd = pusty}
        | (true, false) ->
                {fst = {min = neg_infinity; max = w.fst.max /. z.fst.max};
                 snd = {min = w.snd.min /. z.fst.min; max = infinity}}
        | (false, true) ->
                let z = odwrotnosc z in wyklucz (razy w z)
                (* {fst = {min = neg_infinity; max = z.fst.max /. w.fst.max};
                 snd = {min = z.snd.min /. w.fst.min; max = infinity}} *)
        | (false, false) ->
                let part_b =
                    if (sgn z.fst.min *. sgn z.fst.max > 0.) then
                        {fst = {min = 1. /. z.fst.max; max = 1. /. z.fst.min};
                         snd = pusty}
                    else if (sgn z.fst.min *. sgn z.fst.max < 0.) then
                        {fst = {min = neg_infinity; max = 1. /. z.fst.min};
                         snd = {min = 1. /. z.fst.max; max = infinity}}
                    else if (z.fst.min = 0.) then
                        {fst = {min = 1. /. z.fst.max; max = infinity};
                         snd = pusty}
                    else
                        {fst = {min = neg_infinity; max = 1. /. z.fst.min};
                         snd = pusty}
                in razy w part_b
;;

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s;;                        (* (-inf, 0) *)

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d      (* (-inf, 2> U <4 inf) *)
let f = podzielic b e (* (-inf, 1/4> U <1/2, inf) *)
let g = podzielic d a (* (-inf, -3> U <3, inf) *)
let h = podzielic g f (* (-inf, inf *)
let i = plus f g;;    (* (-inf, inf) *)

assert ((in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49, in_wartosc f 0.50)=(true, false, false, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.) = (neg_infinity, infinity, true, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3) = (neg_infinity, infinity, true, true));;

