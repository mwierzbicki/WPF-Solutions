(*****************************)
(* Autor: Marcin Wierzbicki. *)
(*****************************)

(* przedzialy sa przechowywane w ponizszej postaci. gdy przedzial jest pojedynczy, trzymana jest jego kopia *)
type wartosc = (float * float) * (float * float)
let wnan = (nan , nan) , (nan , nan)

let ff w = fst (fst w) (* 1 element 1 pary *)
let fs w = snd (fst w) (* 2 element 1 pary *)
let sf w = fst (snd w) (* 1 element 2 pary *)
let ss w = snd (snd w) (* 2 element 2 pary *)

(* sprawdza czy przedzial jest NaN *)
let is_nan a = (compare (ff a) nan == 0) && (compare (fs a) nan == 0)
(* sprawdza czy przedzial jest pojedynczy a nie suma *)
let single w = (compare (ff w) (sf w) = 0) && (compare (fs w) (ss w) = 0)
(* duplikuje pojedynczy przedzial - kwestia implementacji *)
let dupl w = (w , w)
(* sprawdza czy przedzial jest singletonem (wartosc_dokladna) *)
let singleton w x = (compare x (ff w) = 0) && (compare x (fs w) = 0)

let wartosc_dokladnosc x p =
  let pr = x *. (p /. 100.0)
  in let mmin = min (x -. pr) (x +. pr)
  in let mmax = max (x -. pr) (x +. pr)
  in dupl (mmin , mmax)

let wartosc_od_do x y = dupl (x , y)

let wartosc_dokladna x = wartosc_od_do x x

let in_wartosc w x =
  if (is_nan w) then false else 
  ((x >= ff w && x <= fs w)) || ((x >= sf w && x <= ss w))

let min_wartosc x = ff x
let max_wartosc x = ss x
let sr_wartosc x = (ff x +. ss x) /. 2.0

(* funkcja probujaca laczyc 2 pojedyncze przedzialy, zwraca caly przedzial *)
let try_merge a b =
  if (fst a <= snd b) && (fst b <= snd a) then dupl (min (fst a) (fst b) , max (snd a) (snd b))
  else if (fst a > fst b) then (b, a)
  else (a, b)

(* funkcja pomocnicza - dodawanie na pojedynczych przedzialach *)
let plus_pom a b = (fst a +. fst b , snd a +. snd b)

let plus a b =
  if (is_nan a) || (is_nan b) then wnan
  else if (not (single a) && not (single b)) then dupl (neg_infinity , infinity)
  else if single a then try_merge (plus_pom (fst a) (fst b)) (plus_pom (fst a) (snd b))
  else if single b then try_merge (plus_pom (fst a) (fst b)) (plus_pom (snd a) (snd b))
  else dupl (plus_pom (fst a) (fst b))

(* funkcja pomocnicza - odejmowanie na pojedynczych przedzialach *)
let minus_pom a b = (fst a -. snd b , snd a -. fst b)

let minus a b = 
  if (is_nan a) || (is_nan b) then wnan
  else if (not (single a) && not (single b))  then dupl (neg_infinity, infinity)
  else if (single a && not (single b))   then try_merge (minus_pom (fst a) (fst b)) (minus_pom (fst a) (snd b))
  else if (not (single a) && (single b)) then try_merge (minus_pom (fst a) (fst b)) (minus_pom (snd a) (snd b)) 
  else dupl (minus_pom (fst a) (fst b))

(* zamienia ujemne zera na normalne *)
let fix_zero x = if ((compare x (-0.0)) = 0) then 0.0 else x

(* funkcja minimum poprawnie uwzgledniajaca nan *)
let mymin a b =
  if (compare a nan == 0) then b
  else if (compare b nan == 0) then a
  else min a b

(* funkcja maximum poprawnie uwzgledniajaca nan *)
let mymax a b = 
  if (compare a nan == 0) then b
  else if (compare b nan == 0) then a
  else max a b

(* funkcja pomocnicza - mnozenie na pojedynczych przedzialach *)
let razy_pom a b =
  let c = fix_zero (fst a *. fst b)
  in let d = fix_zero (fst a *. snd b)
  in let e = fix_zero (snd a *. fst b)
  in let f = fix_zero (snd a *. snd b)
  in let mmin = mymin (mymin (c) (d)) (mymin (e) (f))
  in let mmax = mymax (mymax (c) (d)) (mymax (e) (f))
  in (mmin , mmax)

let razy a b =
  if (is_nan a) || (is_nan b) then wnan
  else if (singleton a 0.0) || (singleton b 0.0) then dupl (0.0, 0.0)
  else if (not (single a) && not (single b)) then (neg_infinity , max (fs a *. sf b) (sf a *. fs b)) , (min (fs a *. fs b) (sf a *. sf b), infinity)
  else if (single a && not (single b)) then try_merge (razy_pom (fst a) (fst b)) (razy_pom (fst a) (snd b))
  else if (not (single a) && single b) then try_merge (razy_pom (fst b) (fst a)) (razy_pom (fst b) (snd a)) 
  else dupl (razy_pom (fst a) (fst b))

(* zwraca odwrotnosc przedzialu - funkcja wykorzystywana do dzielenia *)
let odwrotnosc a =
  if (is_nan a) then wnan
  else if singleton a 0.0 then wnan
  else if (not (single a)) && (((fs a) *. (sf a)) < 0.0) then dupl (1.0 /. (fs a), 1.0 /. (sf a))
  else if (not (single a)) then (neg_infinity , mymin (1.0 /. fs a) (1.0 /. sf a)) , (mymax (1.0 /. fs a) (1.0 /. sf a), infinity)
  else if ((compare (ff a) 0.0) = 0) then dupl (1.0 /. (fs a) , infinity)
  else if ((compare (fs a) 0.0) = 0) then dupl (neg_infinity , 1.0 /. (ff a))
  else if ((ff a) *. (fs a)) < 0.0 then (neg_infinity , 1.0 /. (ff a)) , (1.0 /. (fs a), infinity)
  else dupl (1.0 /. (fs a), 1.0 /. (ff a))

let podzielic a b = 
  if (is_nan a) || (is_nan b) || (singleton b 0.0) then wnan
  else razy a (odwrotnosc b)
