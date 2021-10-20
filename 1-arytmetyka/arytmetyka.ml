(*       **********************************************************         *)
(*              Zadanie o arytmetyce niedokładnych wartości.                *)
(*              Kod: Adam Greloch (438473)                                  *)
(*              Review: Marcin Żołek (438836)                               *)
(*       **********************************************************         *)

(* ----------------- Definicje typów i funkcje wewnętrzne ----------------- *)

open Float

(** 
    Podstawowy typ przechowywania niedokładnych wartości obsługujący
    dwuprzedziałowość. Wartość dwuprzedziałowa zawsze będzie zbiorem R z
    wyłączeniem określonego przedziału [a,b], dlatego wystarczy przechowywać
    sam przedział [min, max] i określić, czy jest to przedział liczby (dwa =
    false) czy jego dopełnienie (dwa = true).
*)
type wartosc =
    {
        min: float;
        max: float;
        dwa: bool
    }

let pusty =
    {
        min = nan;
        max = nan;
        dwa = false
    }

(** Redefinicja operacji mnożenia, uwzględniająca dla uproszczenia
(neg_)infinity * 0.0 = 0.0. *)
let ( *. ) a b =
    if (a = 0. || b = 0.) then
        0.
    else
        a *. b
;;


(** Zwraca znak floata *)
let sgn n =
    match classify_float n with
    | FP_infinite when n = infinity -> 1.
    | FP_infinite -> (-1.)
    | FP_zero -> 0.
    | _  -> (n /. abs_float(n))
;;

(** Sprawdza, czy x: wartosc jest zbiorem pustym *)
let czy_pusty x = classify_float x.min = FP_nan;;

(** Zwraca odwrotność wartości *)
let odwrotnosc x =
    let a = 1. /. x.min and b = 1. /. x.max in
    {min = min a b; max = max a b; dwa = not x.dwa}
;;

(** Zwraca przeciwność wartości *)
let przeciwnosc x = {min = (-1.) *. x.max; max = (-1.) *. x.min; dwa = x.dwa};;

(** Zwraca dopełnienie przedziału x *)
let dopelnienie x = {min = x.min; max = x.max; dwa = not x.dwa};;
  
(** Przyjmuje dwuprzedziałową liczbę i zwraca złączony jeden, jeśli przedziały
się przecinają *)
let zlacz w =
    if (czy_pusty w) then
        pusty
    else
    if (w.min >= w.max) then
        if (w.dwa) then {min = neg_infinity; max = infinity; dwa = false}
        else {min = w.min; max = w.max; dwa = false}
    else
        w
;;

(** Zwraca sumę (w rozumieniu teorii zbiorów) dwóch dwuprzedziałowych liczb *)
let suma w z = zlacz {min = max w.min z.min; max = min w.max z.max; dwa = true};;

(* ----------------------------- Konstruktory ----------------------------- *)

let wartosc_dokladnosc x p =
    let a = x *. (1. -. 0.01 *. p)
    and b = x *. (1. +. 0.01 *. p)
    in
    {min = min a b; max = max a b; dwa = false}
;;

let wartosc_od_do x y = {min = x; max = y; dwa = false};;

let wartosc_dokladna x = {min = x; max = x; dwa = false};;

(* ------------------------------ Selektory ------------------------------- *)

let in_wartosc w x =
    if (w.min <= x && x <= w.max && not w.dwa)
    then
        true
    else
        if ((x <= w.min || w.max <= x) && w.dwa)
        then
            true
        else
            false
;;

let min_wartosc w = if w.dwa then neg_infinity else w.min;;

let max_wartosc w = if w.dwa then infinity else w.max;;

let sr_wartosc w = if w.dwa then nan else (w.min +. w.max) /. 2.;;

(* ------------------------------ Operatory ------------------------------- *)

let rec plus w z =
    match (w.dwa, z.dwa) with
    | (true, true) ->
            (* suma dwóch wartości dwuprzedziałowych daje R *)
            {min = neg_infinity; max = infinity; dwa = false}
    | (true, false) -> plus z w
    | (false, true) ->
            zlacz {min = z.min +. w.max; max = z.max +. w.min; dwa = true}
    | (false, false) ->
            {min = w.min +. z.min; max = w.max +. z.max; dwa = false}
;;

let minus w z =
    match (w.dwa, z.dwa) with
    | (true, true) ->
            (* różnica dwóch wartości dwuprzedziałowych daje R *)
            {min = neg_infinity; max = infinity; dwa = false}
    | (false, true) | (true, false) ->
            plus w (przeciwnosc z)
    | (false, false) ->
            {min = w.min -. z.max; max = w.max -. z.min; dwa = false}
;;

let rec razy w z =
    if (czy_pusty w || czy_pusty z) then
        pusty
    else
        match (w.dwa, z.dwa) with
        | (true, true) ->
                odwrotnosc (razy (odwrotnosc w) (odwrotnosc z))
        | (true, false) -> razy z w
        | (false, true) ->
                (* dać komentarz odnośnie tej rekurencji *)
                if (w.min = w.max) then
                    if (w.min = 0.) then
                        {min = 0.; max = 0.; dwa = false}
                    else
                        let a = z.min *. w.min
                        and b = z.max *. w.min
                        in
                            {min = min a b; max = max a b; dwa = true}
                else
                    if (sgn w.min *. sgn w.max < 0.) then
                        {min = neg_infinity; max = infinity; dwa = false}
                    else
                        suma (razy (wartosc_dokladna w.min) z) (razy (wartosc_dokladna
                        w.max) z)
        | (false, false) ->
                let a_min = min (w.min *. z.min) (w.min *. z.max)
                and b_min = min (w.max *. z.min) (w.max *. z.max)
                and a_max = max (w.min *. z.min) (w.min *. z.max)
                and b_max = max (w.max *. z.min) (w.max *. z.max)
                in
                    {min = min a_min b_min; max = max a_max b_max; dwa = false}
;;

let rec podzielic w z =
    if (z.min = 0. && z.max = 0. || czy_pusty w || czy_pusty z) then
        pusty
    else if (z.min = neg_infinity && z.max = infinity) then
        if (w.min = 0. && w.max = 0.) then
            wartosc_dokladna 0.
        else
            {min = neg_infinity; max = infinity; dwa = false}
    else
        match (w.dwa, z.dwa) with
        | (true, true) ->
                (* iloczyn dwóch wartości dwuprzedziałowych daje R *)
                {min = neg_infinity; max = infinity; dwa = false}
        | (true, false) ->
                odwrotnosc (razy (odwrotnosc w) z)
        | (false, true) ->
                if (in_wartosc z 0.) then
                    dopelnienie (razy w (odwrotnosc z))
                else
                    razy w (odwrotnosc z)
        | (false, false) ->
                let part_b =
                    if (sgn z.min *. sgn z.max > 0.) then
                        {min = 1. /. z.max; max = 1. /. z.min; dwa = false}
                    else if (sgn z.min *. sgn z.max < 0.) then
                        {min = 1. /. z.min; max = 1. /. z.max; dwa = true}
                    else if (z.min = 0.) then
                        {min = 1. /. z.max; max = infinity; dwa = false}
                    else
                        {min = neg_infinity; max = 1. /. z.min; dwa = false}
                in razy w part_b
;;

(* -------------------------------- Testy --------------------------------- *)

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

let jed = wartosc_dokladna 1.
let a = wartosc_od_do 1. 4.     (* <1.0, 4.0> *)
let b = wartosc_od_do (-.2.) 3. (* <-2.0, 3.0> *)
let c = podzielic a b           (* (-inf, -1/2> U <1/3, inf) *)
let d = podzielic c b           (* (-inf, -1/6> U <1/9, inf) *)
let e = plus d jed              (* (-inf, 5/6> U <10/9, inf) *)
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));; (* 1/9 *)
assert (is_nan (sr_wartosc d));
assert (in_wartosc d 0.12);
assert (not (in_wartosc d 0.));
assert (not (in_wartosc d (-0.125)));
assert (in_wartosc d f);
assert (not (in_wartosc e 1.));;

let a = wartosc_od_do (-2.) 3.
let b = wartosc_od_do 2. 3.
let c = podzielic b a

let rec iteruj f n acc = match n with
    | 0 -> acc
    | n when n > 0 -> iteruj f (n-1) (f acc acc)
    | _ -> acc

    let x = iteruj razy 10 c;;
assert (not (in_wartosc x 0.));;

let a = sr_wartosc ( podzielic ( wartosc_od_do (0.000000) (0.000000) ) (
    podzielic ( wartosc_od_do (-7.600000) (-5.200000) ) ( plus ( podzielic (
        wartosc_dokladnosc (-8.400000) (6.000000) ) ( wartosc_dokladna
        (0.000000) ) ) ( plus ( wartosc_dokladna (-2.000000) ) ( plus (
            wartosc_od_do (-2.600000) (-1.400000) ) ( wartosc_od_do (-8.200000)
            (2.400000) ) ) ) ) ) ) ;;
assert ((classify_float a) == FP_nan);;

let a = in_wartosc ( plus ( wartosc_dokladna (-0.8) ) ( plus ( podzielic (
    wartosc_od_do (-8.2) (3.2) ) ( minus ( plus ( minus ( wartosc_dokladnosc
    (-0.8) (1.2) ) ( wartosc_dokladna (0.8) ) ) ( wartosc_od_do (-0.6) (1.4) )
    ) ( minus ( wartosc_dokladnosc (-0.6) (0.8) ) ( wartosc_od_do (-7.8) (0.0)
    ) ) ) ) ( wartosc_dokladna (-5.0) ) ) ) (-7.8) ;;
assert (a = true);;

let eps = 1e-6;;

let a = wartosc_od_do 3. 7.;;                        (* [3., 7.]                      *)

assert(min_wartosc a = 3.0);;
assert(max_wartosc a = 7.0);;
assert(in_wartosc a 4.);;
assert(not (in_wartosc a 2.));;

let b = wartosc_od_do (-2.) 5.;;                     (* [-2., 5.]                     *)

assert(sr_wartosc b = 1.5);;
assert(min_wartosc b = -2.);;
assert(max_wartosc b = 5.);;
assert(in_wartosc b (-0.));;

let c = podzielic a b;;                              (* [-inf, -1.5] U [0.6, inf]     *)

assert(not (in_wartosc c 0.));;
assert(in_wartosc c 100.);;

let d = podzielic c b;;                              (* [-inf, -0.3] U [0.12, inf]    *)

assert(compare (sr_wartosc d) nan = 0);;
assert(in_wartosc d (-3. /. 10. -. eps));;
assert(not (in_wartosc d (-3. /. 10. +. eps)));;
assert(max_wartosc d = infinity);;
assert(min_wartosc d = neg_infinity);;

let e = plus d (wartosc_dokladna 2.);;               (* [-inf, 1.7] U [2.12, inf]     *)

assert(in_wartosc e 0.);;
assert(in_wartosc e 1.7);;
assert(in_wartosc e 2.12);;
assert(not (in_wartosc e 1.700000000001));;
assert(not (in_wartosc e 2.119999999999));;

let f = razy d b;;                                   (* [-inf, inf]                   *)

assert(in_wartosc f 1000000.231232333);;
assert(in_wartosc f (-3.14159));;
assert(min_wartosc f = neg_infinity);;
assert(max_wartosc f = infinity);;
assert(compare (sr_wartosc f) nan = 0);;
assert(in_wartosc f (-0.2));;
assert(in_wartosc f (0.11));;
assert(in_wartosc f 0.0);;
assert(in_wartosc f (-0.0));;

let c = wartosc_od_do (-2.4) (-2.3);;
let d = wartosc_od_do neg_infinity infinity;;
let e = podzielic c d;;

assert(max_wartosc e = infinity);;

let a = max_wartosc ( podzielic ( podzielic ( wartosc_dokladnosc (-2.2) (5.4) )
( podzielic ( wartosc_od_do (-5.0) (5.4) ) ( podzielic ( razy ( podzielic (
    wartosc_od_do (0.0) (7.4) ) ( wartosc_dokladna (-7.6) ) ) ( wartosc_od_do
    (0.0) (9.8) ) ) ( podzielic ( wartosc_od_do (0.0) (1.8) ) (
        wartosc_dokladna (-8.2) )) ) ) ) ( wartosc_dokladnosc (-1.4) (9.6) ) )
;;

assert (a = infinity);;

let a = max_wartosc ( podzielic ( wartosc_od_do (-5.200000) (-0.400000) ) (
    podzielic ( podzielic ( wartosc_od_do (4.800000) (7.400000) ) (
        wartosc_od_do (-9.400000) (4.400000) ) ) ( wartosc_dokladna (-5.000000)) ) ) ;;
assert (a = 23.8333333333333357);;

let a =
    max_wartosc ( razy ( podzielic ( wartosc_dokladna (0.000000) ) (
    wartosc_od_do (-8.600000) (-1.000000) ) ) ( podzielic ( wartosc_dokladna (1.200000)
    ) ( wartosc_od_do (0.000000) (0.000000) ) ) ) ;;
assert ((classify_float a) == FP_nan);;

let a =
    sr_wartosc ( podzielic ( wartosc_od_do (0.000000) (0.000000) ) ( minus
( podzielic ( razy ( minus ( wartosc_dokladna (0.000000) ) ( wartosc_dokladnosc
(-6.200000) (7.200000) ) ) ( wartosc_od_do (-1.600000) (3.200000) ) ) (
    wartosc_od_do (0.000000) (9.400000) ) ) ( plus ( minus ( wartosc_dokladnosc
    (2.000000) (9.400000) ) ( wartosc_dokladna (9.000000) ) ) ( minus (
        wartosc_dokladna (-5.000000) ) ( wartosc_dokladna (1.600000) ) ) ) ) ) ;;
assert (a = 0.);;

let a = max_wartosc ( plus ( razy ( podzielic ( wartosc_dokladna (-6.600000) )
( wartosc_dokladna (0.000000) ) ) ( wartosc_dokladnosc (6.600000) (4.000000) )
) ( podzielic ( wartosc_od_do (3.000000) (4.400000) ) ( razy ( wartosc_od_do
(-9.200000) (3.200000) ) ( wartosc_od_do (-9.600000) (2.800000) ) ) ) ) ;;

assert ((classify_float a) == FP_nan);;

let a = max_wartosc ( razy ( wartosc_od_do (-7.000000) (2.000000) ) ( podzielic
( wartosc_od_do (0.000000) (1.000000) ) ( minus ( podzielic ( wartosc_dokladna
(1.000000) ) ( wartosc_dokladna (6.000000) ) ) ( podzielic ( wartosc_dokladna
(-7.000000) ) ( minus ( minus ( podzielic ( wartosc_dokladna (0.000000) ) (
    wartosc_dokladna (6.000000) ) ) ( wartosc_dokladnosc (5.000000) (5.000000)
) ) ( wartosc_od_do (-9.000000) (-1.000000) ) ) ) ) ) ) ;;
assert (a = 4.72847682119205359);;

let a = sr_wartosc ( podzielic ( wartosc_od_do (-5.200000) (6.800000) ) ( minus
( razy ( wartosc_od_do (-5.200000) (0.000000) ) ( wartosc_dokladna (0.000000) )
) ( podzielic ( podzielic ( wartosc_dokladna (6.600000) ) ( wartosc_od_do
(5.400000) (6.800000) ) ) ( plus ( wartosc_dokladnosc (4.200000) (6.200000) ) (
    wartosc_od_do (-8.400000) (2.600000) ) ) ) ) ) ;;
(* assert (a = -5.81948121212120739);; *)
