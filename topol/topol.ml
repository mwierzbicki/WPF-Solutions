(*****************************)
(* Autor: Marcin Wierzbicki. *)
(*****************************)

exception Cykliczne

(* moj strzal ze wiecej nie trzeba wnioskujac po oczekiwanej zlozonosci programu (log), najwyzej nastapi realokacja *)
let hashtbl_size = 1000000

(* wypelnia obie hashmapy mapowaniami z 'a -> int i int -> 'a *)
let fill_hashtables g mtbl rmtbl = let num = ref 0 in
  (* sprobuj dodac, jak da sie to dodaj do obu tablic *)
  let checker x = try ignore (Hashtbl.find mtbl x) with Not_found ->
    begin
      Hashtbl.add mtbl x !num;
      Hashtbl.add rmtbl !num x;
      incr num
    end
  in List.iter (fun x -> checker (fst x); List.iter checker (snd x)) g;
  !num

(* zwraca array postaci a.(x) = [y1,y2,...,yn] gdzie x jest polaczony poszczegolnie z yi *)
let construct_remapped_graph g mtbl arrsize =
  let ans = Array.make arrsize [] in
  let rec helper g mtbl =
    match g with
    | [] -> ()
    | hd::tl -> let arrpos = Hashtbl.find mtbl (fst hd) in
    List.iter (fun x ->
    let nval = (Hashtbl.find mtbl x) in 
    ans.(arrpos) <- nval::ans.(arrpos)) (snd hd); helper tl mtbl
  in helper g mtbl; ans

(* funkcja oplatajaca, robi toposort uzywajac dfs *)
let dfscontrol arrg =
  let odw = Array.make (Array.length arrg) 0 in
  let ans = ref [] in
  let rec dfs x =
    if (odw.(x) = 1) then raise Cykliczne
    else if (odw.(x) = 0) then begin
      odw.(x) <- 1;
      List.iter (fun a -> dfs a) arrg.(x);
      ans := x::(!ans);
      odw.(x) <- 2;
    end
  in
  for i=0 to (Array.length arrg - 1) do
    dfs i
  done;
  !ans

(* sortuje topologicznie, amortyzowane O(n+m), gdzie n to ilosc wierzcholkow a m to ilosc krawedzi *)
let topol g =
  let mapping   = Hashtbl.create ~random:true hashtbl_size in
  let remapping = Hashtbl.create ~random:true hashtbl_size in
  let arrsize = fill_hashtables g mapping remapping in
  List.map (fun x -> Hashtbl.find remapping x) (dfscontrol (construct_remapped_graph g mapping arrsize))
  
