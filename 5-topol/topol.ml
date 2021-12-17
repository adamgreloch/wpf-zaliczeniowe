exception Cykliczne


let topol lista =
    let preprocess acc (v,l) =
        (* Sprawdzam, czy nie nadpisuję poprzednio znalezionych krawędzi z [v] *)
        let edges = if PMap.mem v acc then
                let (_,_,t) = (PMap.find v acc) in l @ t
            else l
        in
        (* krotka: (czy_obejrzany, czy_w_sorted, lista_sasiadow) *)
        PMap.add v (false, false, edges) acc

    in
    let sorted = ref []
    and visited = ref (List.fold_left preprocess (PMap.create compare) lista)
    in
    let rec dfs v =
        let try_to_find =
            if PMap.mem v !visited then
                try (PMap.find v !visited) with Not_found -> (true, true, [])
            else
                (false, false, [])
        in
        match try_to_find with
        | (true, true, _) -> ()
        | (true, false, _) -> raise Cykliczne
        | (_, _, l) ->
                visited := PMap.add v (true, false, l) !visited;
                List.iter dfs l;
                visited := PMap.add v (true, true, l) !visited;
                sorted := v :: !sorted
    in
    List.iter (fun (v,_) -> dfs v) lista;
    !sorted
;;



