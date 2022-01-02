(*
 * Sortowanie topologiczne
 * Code: Adam Greloch
 * Review: Kamil Pilkiewicz (jesteśmy w trójce z Konradem Obernikowiczem)
 *)

exception Cykliczne

let topol lista =
    let preprocess acc (v,l) =
        (* Sprawdzam, czy nie nadpisuję powiązania [v] z listą jego poprzednio
           znalezionymi sąsiadów. Ma to znaczenie w przypadku, gdy ten sam
           wierzchołek [v] w liście wejściowej występuje kilka razy np. z
           różnymi zestawami krawędzi. *)
        let edges = if PMap.mem v acc then
                let (_,_,t) = (PMap.find v acc) in l @ t
            else l
        in
        PMap.add v (false, false, edges) acc

    in
    let sorted = ref []
    and visited = ref (List.fold_left preprocess (PMap.create compare) lista)
    (* [visited] składa się z krotek (bool * bool * a' list):
       (czy_obejrzany, czy_w_sorted, lista_sasiadow) *)
    in
    let rec dfs v =
        let try_to_find =
            if PMap.mem v !visited then
                try (PMap.find v !visited) with Not_found -> (true, true, [])
            else (false, false, [])
        in
        match try_to_find with
        | (true, true, _) -> ()
        | (true, false, _) ->
                (* Jeśli [v] zostało już obejrzane, ale nie ma go w [sorted],
                   to znaczy, że po drodze [v] wskazał na [v], czyli mamy cykl. *)
                raise Cykliczne
        | (_, _, l) ->
                begin
                    visited := PMap.add v (true, false, l) !visited;
                    List.iter dfs l;
                    visited := PMap.add v (true, true, l) !visited;
                    sorted := v :: !sorted
                end
    in
    begin
        List.iter (fun (v,_) -> dfs v) lista;
        !sorted
    end

