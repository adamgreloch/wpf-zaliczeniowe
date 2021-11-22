(*
 * ISet - Interval sets
 * Code: Adam Greloch (438473)        
 * Review: Piotr Trzaskowski (438782)
 *)

type t =
    (* t: drzewo binarne pamiętające wysokość i liczbę elementów. *)
    | Empty
    | Node of t * (int * int) * t * int * int

let empty = Empty

let is_empty s = 
    s = Empty

let (++) a b =
    (* [a ++ b] zapobiega przekroczeniu zakresu podczas operacji dodawania
    dwóch intów i zwraca jej wynik. Działa przy założeniu, że b >= 0. *)
    if a + b < a then max_int
    else a + b

let (--) a b =
    (* [a -- b] zapobiega przekroczeniu zakresu podczas operacji odejmowania
    dwóch intów i zwraca jej wynik. Działa przy założeniu, że a >= b. *)
    if a - b < 0 then max_int
    else a - b

let height = function
    | Node (_, _, _, h, _) -> h
    | Empty -> 0

let elenum = function
    | Node (_, _, _, _, e) -> e
    | Empty -> 0

let make l ((a,b) as v) r =
    (* [make l (a,b) r] łączy w jedno drzewo poddrzewo lewe [l], przedział
    [[a,b]] jako wierzchołek i poddrzewo [r]. Zakłada, że [[a,b]] nie występuje
    w żadnym z wejściowych poddrzew. *)
    let e = b -- a ++ 1 ++ elenum l ++ elenum r
    in
    Node (l, v, r, max (height l) (height r) + 1, e)

let bal l v r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, lv, lr, _, _) ->
            if height ll >= height lr then make ll lv (make lr v r)
            else
                (match lr with
                | Node (lrl, lrv, lrr, _, _) ->
                    make (make ll lv lrl) lrv (make lrr v r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
        match r with
        | Node (rl, rv, rr, _, _) ->
            if height rr >= height rl then make (make l v rl) rv rr
            else
                (match rl with
                | Node (rll, rlv, rlr, _, _) ->
                    make (make l v rll) rlv (make rlr rv rr)
                | Empty -> assert false)
        | Empty -> assert false
    else make l v r

(* Procedury zwracające elementy minimalne, maksymalne oraz je usuwające. *)

let rec min_elt = function
    | Node (Empty, i, _, _, _) -> i
    | Node (l, _, _, _, _) -> min_elt l
    | Empty -> raise Not_found

let rec remove_min_elt = function
    | Node (Empty, _, r, _, _) -> r
    | Node (l, v, r, _, _) -> bal (remove_min_elt l) v r
    | Empty -> invalid_arg "ISet.remove_min_elt"

let rec max_elt = function
    | Node (_, i, Empty, _, _) -> i
    | Node (_, _, r, _, _) -> max_elt r
    | Empty -> raise Not_found

let rec remove_max_elt = function
    | Node (l, _, Empty, _, _) -> l
    | Node (l, v, r, _, _) -> bal l v (remove_max_elt r)
    | Empty -> invalid_arg "ISet.remove_max_elt"

let merge t1 t2 =
    (* [merge t1 t2] zwraca zbalansowane drzewo powstałe z dwóch rozłącznych
    poddrzew [t1] [t2]. *)
    match (t1, t2) with
    | (Empty, _) -> t2
    | (_, Empty) -> t1
    | _ ->
        let v = min_elt t2 in
        bal t1 v (remove_min_elt t2)

let rec add_one ((a,b) as x) s =
    (* [add_one (a,b) s] dodaje do zbioru [s] przedział [[a,b]]. Zakłada, że
    wejściowe zbiory mają puste przecięcie. *)
    if a > b then s else
    match s with
    | Node (l, ((c,d) as v), r, _, _) ->
        if b < c then
            make (add_one x l) v r
        else
            make l v (add_one x r)
    | Empty -> make Empty x Empty

let rec join l v r =
    (* [make l (a,b) r] tworzy zbalansowane drzewo składające się z poddrzewa
    lewego [l], przedziału [[a,b]] w wierzchołku i poddrzewa [r]. Zakłada, że
    [[a,b]] nie występuje w żadnym z wejściowych poddrzew. *)
    match (l, r) with
    | (Empty, _) -> add_one v r
    | (_, Empty) -> add_one v l
    | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
        if lh > rh + 2 then
            bal ll lv (join lr v r)
        else if rh > lh + 2 then
            bal (join l v rl) rv rr
        else
            make l v r

let rec split a = function
    | Empty -> (Empty, false, Empty)
    | Node (l, ((b, c) as v), r, _, _) ->
        if a < b then
            let (ll, pres, rl) = split a l in (ll, pres, join rl v r)
        else if b <= a && a <= c then
            let nl = if a = b then l else add_one (b, a-1) l
            and nr = if a = c then r else add_one (a+1, c) r
            in
                (nl, true, nr)
        else
            let (lr, pres, rr) = split a r in (join l v lr, pres, rr)

let rec add (a,b) s =
    (* [add (a,b) s] zwraca zbiór zawierający elementy zbioru [s] plus elementy
       z przedziału [[a,b]]. Zakłada, że [a <= b]. Oprócz tego przedział
       [[a,b]] może być dowolny. *)
    let (al, _, _) = split a s
    and (_, _, br) = split b s
    in
    let (lc, ld) = if al = Empty then (a,a) else max_elt al
    and (rc, rd) = if br = Empty then (b,b) else min_elt br
    in
    let (al,a) = if al <> Empty && (ld = a || ld = a-1) then
        (remove_max_elt al, lc) else (al, a)
    and (br,b) = if br <> Empty && (rc = b || rc = b+1) then
        (remove_min_elt br, rd) else (br, b)
    in
        join al (a,b) br


let remove (a,b) s = 
    (* [remove (a,b) s] zwraca zbiór zawierający elementy zbioru [s] z
       wyjątkiem elementów z przedziału [[a,b]]. Zakłada, że [a <= b]. Oprócz
       tego przedział [[a,b]] może być dowolny. *)
    let (al, _, _) = split a s
    and (_, _, br) = split b s
    in
    merge al br

let rec mem a = function
    | Node (l, (b,c), r, _, _) ->
        if b <= a && a <= c then true
        else if a < b then
            mem a l
        else
            mem a r
    | Empty -> false

let iter f s =
    let rec loop = function
        | Empty -> ()
        | Node (l, k, r, _, _) -> loop l; f k; loop r
    in
    loop s

let fold f s acc =
    let rec loop acc = function
        | Empty -> acc
        | Node (l, k, r, _, _) ->
            loop (f k (loop acc l)) r
    in
    loop acc s

let elements s = 
    let rec loop acc = function
        | Empty -> acc
        | Node(l, k, r, _, _) -> loop (k :: loop acc r) l
    in
    loop [] s

let below n s =
    let (l, pres, _) = split n s
    in
    elenum l ++ (if pres then 1 else 0)

