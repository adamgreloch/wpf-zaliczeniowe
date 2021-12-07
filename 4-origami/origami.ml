(*
 * Origami
 * Code: Adam Greloch
 * Review: Maja Tkaczyk
 *)

let eps = 1e-12

type point = float * float

type kartka = point -> int

let prostokat (x1,y1) (x2,y2) =
    function (x,y) -> if (x1 <= x && x <= x2 && y1 <= y && y <= y2) then 1 else 0

let kolko (x0,y0) r =
    function (x,y) -> if (sqrt ((x0 -. x)**2. +. (y0 -. y)**2.) <= r +. eps) then 1 else 0

let obraz (x,y) (x1,y1) (x2,y2) =
    (* [obraz p p1 p2] zwraca obraz punktu [p] w symetrii względem prostej
       wyznaczonej przez punkty [p1], [p2]. By zatem istniała prosta, [p1] i
       [p2] muszą być różne. Obraz otrzymywany jest metodą polegającą na
       znalezieniu prostej prostopadlej do [p1p2], znalezieniu punktu
       przecięcia [s]. Translacja [s] o wektor [ps] daje nam szukany obraz. *)
    if x1 = x2 then (2. *. x1 -. x, y)
    else if y1 = y2 then (x, 2. *. y1 -. y) 
    else
    let (a,c) = ((y1 -. y2) /. (x1 -. x2), (x2 -. x1) /. (y1 -. y2)) in
    let (b,d) = (y2 -. a *. x2, y -. c *. x) in
    let sx = (d -. b) /. (a -. c) in
    let sy = sx *. a +. b in 
    (2. *. sx -. x, 2. *. sy -. y)

type polozenie = Lewo | Prosta | Prawo
    (* typ [polozenie] do wyznaczania lokalizacji punktu względem prostej
       [Lewo], [Prawo] - punkt leży po lewej/prawej stronie prostej,
       [Prosta] - punkt należy do prostej *)

let gdzie_jest (x,y) (x1,y1) (x2,y2) =
    (* [gdzie_jest p p1 p2] odpowiada na pytanie "gdzie jest punkt [p] względem
       prostej wyznaczonej przez punkty [p1] i [p2]?" na podstawie znaku wartości
       iloczynu wektorowego między wektorami [p2,p1], [p2,p]. *)
    let p = (x -. x2) *. (y2 -. y1) -. (y -. y2) *. (x2 -. x1) in
    if abs_float p < eps then Prosta
    else if p < 0. then Lewo
    else Prawo

let zloz p1 p2 k = function p0 ->
    (* [zloz p1 p2 k] zwraca złożenie kartki [k] wzdłuż prostej [p1p2].
       Zakłada, że [p1], [p2] są różne. Jeśli [p0] jest po lewej stronie
       prostej wyznaczonej przez [p1] i [p2] to znaczy, że przebija złożony
       fragment kartki oraz to, co pod nią z niej zostało. Jeśli jest na
       prostej, to przebija wyłącznie brzeg, a jeśli na prawo, to przebija
       powietrze. *)
    match gdzie_jest p0 p1 p2 with
    | Lewo -> k p0 + k (obraz p0 p1 p2)
    | Prosta -> k p0
    | Prawo -> 0
;;

let skladaj l k = List.fold_left (fun k (p1,p2) -> zloz p1 p2 k) k l

