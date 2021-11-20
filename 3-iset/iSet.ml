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
    (* TODO: obsługa nieskończoności *)
    let e = (-a) ++ b ++ elenum l ++ elenum r ++ 1 in
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
            (add_one (b,a-1) l, true, add_one (a+1,c) r)
        else
            let (lr, pres, rr) = split a r in (join l v lr, pres, rr)

let rec add ((a,b) as x) = function
    (* TODO: obsługa nieskończoności *)
    | Empty -> make Empty x Empty
    | Node (l, (c,d), r, _, _) as s ->
        if c <= a && b <= d then s else
        let (a,b) = if b = c || b ++ 1 = c then (a,d)
            else if a = d || a ++ (-1) = d then (c,b)
            else (a,b)
        in
        let (al, apres, _) = split (a-1) s
        and (_, bpres, br) = split (b+1) s
        in
        let a = if apres then a-1 else a
        and b = if bpres then b+1 else b
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

