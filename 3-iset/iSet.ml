(* ISet - Interval sets *)

type interval = int * int

type set =
    | Empty
    | Node of set * interval * set * int

type t =
    {
        cmp : interval -> interval -> int;
        set : set;
    }

let height = function
    | Node (_, _, _, h) -> h
    | Empty -> 0

let make l i r = Node (l, i, r, max (height l) (height r) + 1)

let bal l i r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, li, lr, _) ->
            if height ll >= height lr then make ll li (make lr i r)
            else
                (match lr with
                | Node (lrl, lri, lrr, _) ->
                          make (make ll li lrl) lri (make lrr i r)
                | Empty -> assert false)
        | Empty -> assert false
    else if hr > hl + 2 then
        match r with
            | Node (rl, ri, rr, _) ->
                if height rr >= height rl then make (make l i rl) ri rr
                else
                    (match rl with
                      | Node (rll, rli, rlr, _) ->
                          make (make l i rll) rli (make rlr ri rr)
                      | Empty -> assert false)
            | Empty -> assert false
    else Node (l, i, r, max hl hr + 1)

let rec min_elt = function
    | Node (Empty, i, _, _) -> i
    | Node (l, _, _, _) -> min_elt l
    | Empty -> raise Not_found

let rec remove_min_elt = function
    | Node (Empty, _, r, _) -> r
    | Node (l, i, r, _) -> bal (remove_min_elt l) i r
    | Empty -> invalid_arg "PSet.remove_min_elt"

let merge t1 t2 =
    match (t1, t2) with
    | (Empty, _) -> t2
    | (_, Empty) -> t1
    | _ ->
        let i = min_elt t2 in
        bal t1 i (remove_min_elt t2)

let is_equal (min1,max1) (min2,max2) =
    min1 = min2 && max1 = max2

let compare_i (min1,max1) (min2,max2) =
    if max1 < min2 then
        -2 (* rozłączne, i1 jest po lewej stronie i2 na osi liczbowej *)
    else if min2 <= min1 && max1 <= max2 then
        0 (* i1, i2 są tym samym przedziałem bądź i1 zawiera się w i2 *)
    else if min1 <= min2 && max2 <= max1 then
        -1 (* i2 zawiera się w i1 *)
    else if max2 >= min1 && min2 <= max1 then
        1 (* i1, i2 mają część wspólną *)
    else
        2 (* rozłączne, i2 jest po lewej stronie i1 na osi liczbowej *)

let join_i (min1,max1) (min2,max2) = (min min1 min2, max max1 max2)

let exclude_i (min1,max1) (min2,max2) =
    (* funkcja odejmująca od i2 przedzial i1 *)
    match compare_i (min1,max1) (min2,max2) with
    | 0 ->
        if min1 = min2 || max1 = max2 then
            let res = (max1+1,max2) in (res,res)
        else
            ((min2,min1-1),(max1+1,max2))
    | 1 ->
        let res = if min1 < max2 then
            (max1+1,max2)
        else
            (min2,min1-1)
        in
        (res,res)
    | _ -> failwith "impossible"

let create cmp = { cmp = cmp; set = Empty }
let empty = { cmp = compare_i; set = Empty }

let is_empty x = 
    x.set = Empty

let rec add_one cmp x = function
    | Node (l, i, r, h) ->
        let c = cmp x i in
        if c = 0 then Node (l, i, r, h)
        else if c = -2 then
            let nl = add_one cmp x l in
            bal nl i r
        else if c = 2 then
            let nr = add_one cmp x r in
            bal l i nr
        else
            let nr = add_one cmp (join_i x i) r in
            merge l nr
    | Empty -> Node (Empty, x, Empty, 1)

let add x { cmp = cmp; set = set } =
    { cmp = cmp; set = add_one cmp x set }

let rec join cmp l v r =
    match (l, r) with
    | (Empty, _) -> add_one cmp v r
    | (_, Empty) -> add_one cmp v l
    | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
        if lh > rh + 2 then bal ll lv (join cmp lr v r) else
            if rh > lh + 2 then bal (join cmp l v rl) rv rr else
                make l v r

let split x { cmp = cmp; set = set } =
    let rec loop x = function
        | Empty ->
            (Empty, false, Empty)
        | Node (l, v, r, _) ->
            let c = cmp x v in
            if c = 0 then (l, true, r)
            else if c < 0 then
                let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl v r)
            else
                let (lr, pres, rr) = loop x r in (join cmp l v lr, pres, rr)
    in
    let setl, pres, setr = loop x set in
    { cmp = cmp; set = setl }, pres, { cmp = cmp; set = setr }

let rec remove x { cmp = cmp; set = set } =
    let rec loop nx = function
        | Node (l, i, r, _) ->
            let c = cmp i nx in
            if c = 0 then
                if is_equal i nx then
                    merge l r
                else
                    let (res1, _) = exclude_i i nx in
                    loop res1 (merge l r)
            else if c = 2 then
                bal (loop nx l) i r
            else if c = 1 then
                let (res1, _) = exclude_i nx i in
                loop res1 (merge l r)
            else if c = -1 then
                let (res1,res2) = exclude_i nx i in
                if res1=res2 then
                    join cmp l res1 r
                else
                    bal (add_one cmp res1 l) res2 r
            else
                bal l i (loop nx r)
        | Empty -> Empty
    in
    { cmp = cmp; set = loop x set }

let mem x { cmp = cmp; set = set } =
    let rec loop = function
        | Node (l, i, r, _) ->
                let c = cmp x i in
                c = 0 || loop (if c < 0 then l else r)
        | Empty -> false in
            loop set

let exists = mem

let iter f { set = set } =
    let rec loop = function
        | Empty -> ()
        | Node (l, i, r, _) -> loop l; f i; loop r
    in
    loop set

let fold f { cmp = cmp; set = set } acc =
    let rec loop acc = function
        | Empty -> acc
    | Node (l, i, r, _) ->
            loop (f i (loop acc l)) r
    in
    loop acc set

let elements { set = set } = 
    let rec loop acc = function
        | Empty -> acc
        | Node(l, i, r, _) -> loop (i :: loop acc r) l
    in
    loop [] set

let below n { cmp = cmp; set = set } =
    let rec loop acc = function
        | Node (l, (min1,max1), r, _) ->
            let acc = if min1 <= n then
                n - min1 + 1
            else acc in
            max (loop acc l) (loop acc r)
        | Empty -> acc
    in
    loop 0 set

let s1 = add (1,3) empty |> add (1,2) |> add (4,7) |> add (10,10);;
let t1 = elements s1 = [(1,3); (4,7); (10,10)];;

assert (t1);;
