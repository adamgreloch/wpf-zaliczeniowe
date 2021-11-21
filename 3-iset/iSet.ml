(* ISet - Interval sets *)

type t =
    | Empty
    | Node of t * (int * int) * t * int * int

let (++) a b =
    let (a, b) = (min a b, max a b) in
    if a >= 0 then
        if a < max_int - b then a + b else max_int
    else
        if b > min_int - a then a + b else min_int

let height = function
    | Node (_, _, _, h, _) -> h
    | Empty -> 0

let elenum = function
    | Node (_, _, _, _, e) -> e
    | Empty -> 0

let make l ((a,b) as v) r =
    let e =
    if a = min_int then
        -(a+1) ++ b ++ 2 ++ elenum l ++ elenum r
    else
        (-a) ++ b ++ 1 ++ elenum l ++ elenum r
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
    match (t1, t2) with
    | (Empty, _) -> t2
    | (_, Empty) -> t1
    | _ ->
        let v = min_elt t2 in
        bal t1 v (remove_min_elt t2)

let empty = Empty

let is_empty s = 
    s = Empty

let rec add_one ((a,b) as x) s =
    if a > b then s else
    match s with
    | Node (l, ((c,d) as v), r, _, _) ->
        if b < c then
            make (add_one x l) v r
        else
            make l v (add_one x r)
    | Empty -> make Empty x Empty

let rec join l v r =
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
            if a = min_int then
                (add_one (b,a) l, false, add_one (a++1,c) r)
            else
                (add_one (b,a++(-1)) l, true, add_one (a++1,c) r)
        else
            let (lr, pres, rr) = split a r in (join l v lr, pres, rr)

let rec add ((a,b) as x) = function
    | Empty -> make Empty x Empty
    | Node (_, (c,d), _, _, _) as s ->
        if c <= a && b <= d then s else
        let (al, _, _) = split a s
        and (_, _, br) = split b s
        in
        let (lc, ld) = if al = Empty then (a,a) else max_elt al
        and (rc, rd) = if br = Empty then (b,b) else min_elt br
        in
        let (al,a) = if al <> Empty && (ld = a || ld = a ++ (-1)) then
            (remove_max_elt al, lc) else (al, a)
        and (br,b) = if br <> Empty && (rc = b || rc = b ++ 1) then
            (remove_min_elt br, rd) else (br, b)
        in
            join al (a,b) br

let remove (a,b) s = 
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

let a = add (0, 5) empty;;
let a = add (7, 8) a;;
let a = add (-3, -3) a;;
let a = add (10, 13) a;;
assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
assert(below 8 a = 9);;
let b = add (6, 6) a;;
let b = remove (6, 6) b;;
let b = add (-100, -5) b;;
let b = add (-4, 6) b;;
assert(elements b = [(-100, 8); (10, 13)]);;
assert(below 10 b = 110);;
let c = remove (2, 10) a;;
assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
assert(below 12 c = 5);;

let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Zly wynik testu %d!\n" id;
        incr zle
    end;;

let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 51 (below 2 s = 0) true;;
test 52 (below 3 s = 1) true;;
test 53 (below 10 s = 5) true;;
test 54 (below 15 s = 6) true;;
test 55 (below 100 s = 11) true;;
let s = add (1, max_int) (add (-1, 0) empty);;
test 56 (below max_int s = max_int) true;;
let s = add (-min_int, max_int) empty;;
test 57 (below max_int s = max_int) true;;
test 58 (below min_int s = 1) true;;
