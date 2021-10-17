open Arytmetyka

let jeden = wartosc_dokladna 1.0;;
let zero = wartosc_dokladna 0.0;;
let dodatnie_zero1 = wartosc_od_do 0.0 1.0;;
let ujemne_zero1 = wartosc_od_do (-1.0) 0.0;;
let rozne_znaki1 = wartosc_od_do (-1.0) 1.0;;
let duzo = wartosc_dokladnosc 777777.7 9999.9;;

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

let plus1 = pokaz (plus dodatnie_zero1 ujemne_zero1);;
(* exp (-1., 1., 0) *)

let plus2 = pokaz (plus dodatnie_zero1 zero);;
(* exp (0., 1., 0) *)

let plus3 = pokaz (plus ujemne_zero1 zero);;
(* exp (-1., 0., 0) *)

let plus4 = pokaz (plus ujemne_zero1 rozne_znaki1);;
(* exp (-2., 1., 0) *)

let plus5 = pokaz (plus dodatnie_zero1 rozne_znaki1);;
(* exp (-1., 2., 0) *)

let minus1 = pokaz (minus dodatnie_zero1 ujemne_zero1);;
(* exp (0., 2., 1) *)

let minus1odw = pokaz (minus ujemne_zero1 dodatnie_zero1);;
(* exp (-2., 0., -1) *)

let minus2 = pokaz (minus dodatnie_zero1 zero);;
(* exp (0., 1., 1) *)

let minus2odw = pokaz (minus zero dodatnie_zero1);;
(* exp (-1., 0., 0) *)

let minus3 = pokaz (minus ujemne_zero1 zero);;
(* exp (-1., 0., -1) *)

let minus3odw = pokaz (minus zero ujemne_zero1);;
(* exp (0., 1., 0) *)

let minus4 = pokaz (minus ujemne_zero1 rozne_znaki1);;
(* exp (-2., 1., 0) *)

let minus4odw = pokaz (minus rozne_znaki1 ujemne_zero1);;
(* exp (-1., 2., 0) *)

let minus5 = pokaz (minus dodatnie_zero1 rozne_znaki1);;
(* exp (-1., 2., 0) *)

let minus5odw = pokaz (minus rozne_znaki1 dodatnie_zero1);;
(* exp (-2., 1., 0) *)

let razy1 = pokaz (razy dodatnie_zero1 ujemne_zero1);;
(* exp (-1., -0., -1) *)

let razy2 = pokaz (razy dodatnie_zero1 zero);;
(* exp (0., 0., 0) *)

let razy3 = pokaz (razy ujemne_zero1 zero);;
(* exp (0., 0., 0) *)

let razy4 = pokaz (razy ujemne_zero1 rozne_znaki1);;
(* exp (-1., 1., 0) *)

let razy5 = pokaz (razy dodatnie_zero1 rozne_znaki1);;
(* exp (-1., 1., 0) *)

let podzielic1 = pokaz (podzielic jeden dodatnie_zero1);;
(* exp (1., infinity, 2) *)

let podzielic1odw = pokaz (podzielic dodatnie_zero1 jeden);;

let podzielic2 = pokaz (podzielic jeden ujemne_zero1);; 

let podzielic2odw = pokaz (podzielic ujemne_zero1 jeden);; 
(* exp (neg_infinity., -1, 2) *)

assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.)
&& (in_wartosc c 100000.));;

let sgn n =
    match n with
    | 0. -> 0.
    | _  -> (n /. abs_float(n))
;;

let testuj_min = 
    let minimize a_min (b1, b2, b3, b4) =
        if (classify_float b3 = FP_nan && classify_float b4 = FP_nan) then
            if ((a_min = infinity && (sgn b1 < 0. || sgn b2 < 0.))
                    ||
                    (a_min = neg_infinity && (sgn b1 > 0. || sgn b2 > 0.))
                )
                then
                    neg_infinity
            else
                min (a_min *. b1) (a_min *. b2)
        else
            (* TODO rozszerzyć na dwa przedzialy *)
            min (min (a_min *. b1) (a_min *. b2)) (min (a_min *. b3) (a_min *. b4))
    in
    let y = (-0.0002, 0., nan, nan) in minimize infinity y;;

let testuj_max = 
    let maximize a_max (b1, b2, b3, b4) =
            if (classify_float b3 = FP_nan && classify_float b4 = FP_nan) then
                if (classify_float a_max = FP_infinite) then
                    if ((a_max = infinity && (sgn b1 > 0. || sgn b2 > 0.))
                            ||
                            (a_max = neg_infinity && (sgn b1 < 0. || sgn b2 < 0.))
                        )
                    then
                        infinity
                    else
                        0.
                else
                    max (a_max *. b1) (a_max *. b2)
            else
                (* TODO rozszerzyć na dwa przedzialy *)
                max (max (a_max *. b1) (a_max *. b2)) (max (a_max *. b3) (a_max *. b4))
    in
    let y = (-0.0002, 0., nan, nan) in maximize infinity y;;
