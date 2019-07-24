(*****************************)
(* Autor: Marcin Wierzbicki. *)
(*****************************)

(* NAPRAWIA NIEDOSKONALOSC FUNKCJI HASHUJACEJ W OCAMLU - zeby hashowal caly array *)  
module MyHash =
  struct
    type t = int array
    let equal i j = i=j
    let hash = Hashtbl.hash_param 100 100
  end

module Hashtbl = Hashtbl.Make(MyHash) 

let przelewanka intab =

  (* zeby heurystyka gcd dzialala, usuwa zerowe szklanki *)
  let temp_list = ref [] in
  for i = 0 to (Array.length intab) - 1 do
    if (fst intab.(i) != 0) then temp_list := intab.(i)::!temp_list
  done;
  let info = Array.of_list (List.rev !temp_list) in

  (* deklaracje zmiennych *)
  let size = Array.length info in
  let ans = ref (-1) in 
  let heuristics_pass = ref 0 in
  let cstate = Array.make size 0 in
  let fifo = Queue.create () in
  let htbl = Hashtbl.create 1010101 in

  (* operacje jednostkowe *)
  let fill a st = let nst = Array.copy st in (nst.(a) <- fst info.(a); nst) in
  let empty a st = let nst = Array.copy st in (nst.(a) <- 0; nst) in
  let pour a b st =
    let nst = Array.copy st in
    let h = (fst info.(b)) - nst.(b) in
    if (h >= nst.(a)) then (nst.(b) <- nst.(b) + nst.(a); nst.(a) <- 0; nst)
    else (nst.(a) <- nst.(a) - h; nst.(b) <- fst info.(b); nst) in

  (* sprawdza czy stan jest tym szukanym *)
  let endcheck st = 
    let pass = ref 0 in
    for i = 0 to size - 1 do
      if (snd info.(i) = st.(i)) then incr pass
    done; (size = !pass) in
  
  (* heurystyki - jedna z dzielnikiem (bo inaczej nie uda sie skonstruowac rozwiazania przelewajac),
     druga sprawdza czy ostania szklanka bedzie pelna lub pusta (zeby byla czesc plynu trzeba dodatkowej) *)
  let rec gcd a b =
    if b = 0 then a
    else gcd b (a mod b) in

  let check_gcd =
    let mdiv = Array.fold_left (fun acc (f, _) -> gcd acc f) 0 info in
      Array.for_all (fun (_, s) -> s mod mdiv = 0) info in

  let empty_full_heuristic =
    Array.exists (fun (f, s) -> f = s || s = 0) info in

  (* glowny program *)
  Hashtbl.add htbl cstate 0;
  Queue.push cstate fifo;
  (* heurystyk test *)
  if (check_gcd && empty_full_heuristic) then (heuristics_pass := 1);
  if (endcheck cstate) then (ans := 0)
  else
  while (not (Queue.is_empty fifo) && !ans = -1 && !heuristics_pass = 1) do
    let cstate = Queue.pop fifo in
    let newdist = (Hashtbl.find htbl cstate) + 1 in

    (* generujemy nowe stany do przeszukania *)
    for i = 0 to size - 1 do
      let to_add = ref [] in
      if (cstate.(i) != fst info.(i)) then to_add := (fill i cstate)::(!to_add);
      if (cstate.(i) != 0) then to_add := (empty i cstate)::(!to_add);
      for j = 0 to size - 1 do
        if ((i != j) && (cstate.(i) != 0)) then to_add := (pour i j cstate)::(!to_add);
      done;
      List.iter (fun x ->
        if (not (Hashtbl.mem htbl x)) then (if (endcheck x) then (ans := newdist); Queue.push x fifo; Hashtbl.add htbl x newdist)
      ) !to_add;
    done;
  done;
  !ans
