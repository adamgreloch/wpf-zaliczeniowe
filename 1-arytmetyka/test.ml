open Arytmetyka

let jeden = wartosc_dokladna 1.0;;
let zero = wartosc_dokladna 0.0;;
let dodatnie_zero1 = wartosc_od_do 0.0 1.0;;
let ujemne_zero1 = wartosc_od_do (-1.0) 0.0;;
let rozne_znaki1 = wartosc_od_do (-1.0) 1.0;;
let duzo = wartosc_dokladnosc 777777.7 9999.9;;


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

let podzielic2 = pokaz (podzielic jeden ujemne_zero1);; 
