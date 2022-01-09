(*
 * Przelewanka
 * Code: Adam Greloch
 * Review: Jakub Klimek
 *)

let rec nwd_l res = function
    (* Procedura zwracająca NWD całej listy intów *)
    | [] -> res
    | h::t ->
    let rec nwd a b = if b = 0 then a else nwd b (a mod b) in
    nwd_l (nwd h res) t

open Array

let przelewanka arr = 
    let n = length arr in
    if n = 0 then 0 else
    if exists (fun (x,y) -> x < y) arr then
        (-1)
        (* Szklanka o pojemności x nie może pomieścić więcej niż x wody. *)
    else let d = nwd_l (fst arr.(0)) (to_list (map (fun (x,_) -> x) arr)) in
    (* d := największy wspólny dzielnik wszystkich pojemności szklanek *)
    if d = 0 then
        0
        (* Jeśli d = 0, to mamy n szklanek o zerowej pojemności, więc
           bezwysiłkowy sukces, bo wszystko już . *)
    else if exists (fun (_,y) -> y mod d <> 0) arr then
        (-1)
        (* Robiąc operacje na szklankach zawsze nalewamy/przelewamy ilość wody
           podzielną przez d. Przy pomocy określonych w zadaniu operacji
           niemożliwe jest zatem otrzymanie konfiguracji, w której pewna
           szklanka ma ilość wody niepodzielną przez d. *)
    else if not (exists (fun (x,y) -> x = y || y = 0) arr) then
        (-1)
        (* Conajmniej jedna szklanka musi być na koniec pełna lub pusta. *)
    else
    (* Na tym etapie oczekiwany wynik ma sens. Od teraz możemy stwierdzić
       brak rozwiązania jedynie po sprawdzeniu wszystkich konfiguracji. *)
    let q = ref (Queue.create())
    and visited = Hashtbl.create 424242
    and min_kr = ref (-1) in
    let is_good v = for_all2 (fun x (_,y) -> x = y) v arr in
    let try_to_add (v,kr) =
        if !min_kr = (-1) && not (Hashtbl.mem visited v) then begin
            if is_good v then min_kr := kr else
            begin
                Queue.add (v,kr) !q;
                Hashtbl.add visited v true
            end
        end
    in
    let nalej (v,kr) =
        for i = 0 to n - 1 do
            let (mx,_) = arr.(i)
            and cp = copy v in
            cp.(i) <- mx;
            try_to_add (cp, kr+1)
        done
    in
    let wylej (v, kr) =
        for i = 0 to n - 1 do
            let cp = copy v in
            cp.(i) <- 0;
            try_to_add (cp, kr+1)
        done
    in
    let przelej (v,kr) =
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                if i <> j then begin
                    let cp = copy v
                    and (mx,_) = arr.(i) in
                    if cp.(i) + cp.(j) > mx then begin
                        cp.(j) <- cp.(i) + cp.(j) - mx;
                        cp.(i) <- mx
                        end
                    else begin
                        cp.(i) <- cp.(i) + cp.(j);
                        cp.(j) <- 0
                    end;
                    try_to_add (cp,kr+1)
                end
            done
        done
    in
    let bfs v =
        if (is_good v) then min_kr := 0
        else begin
            nalej (v,0);
            while not (Queue.is_empty !q) do
                let e = Queue.pop !q in
                List.iter (fun f -> f e) [nalej; wylej; przelej];
            done
        end
    in
    bfs (make n 0);
    !min_kr

