(*****************************)
(* Autor: Marcin Wierzbicki. *)
(*****************************)

type point = float * float

type kartka = point -> int

let eps = 100000. *. epsilon_float

(* funkcja do wykonywania dzialan na parach. moze dodawac wektory, odejmowac,
   oraz mnozyc i dzielic przez skalar postaci (x, x) *)
let m (a : point) (b : point) (f : float -> float -> float) : point  = (f (fst a) (fst b) , f (snd a) (snd b))

let para x = (x , x)

(* liczy iloczyn skalarny dla punktu na plaszczyznie i prostej *)
let skalarny (x1, y1) (x2, y2) = (x1 *. x2) +. (y1 *. y2)

(* liczy iloczyn wektorowy *)
let wektorowy (x1, y1) (x2, y2) = (x1 *. y2) -. (x2 *. y1)

(* liczy odleglosc 2 pkt *)
let odleglosc (x1, y1) (x2, y2) = sqrt (((x2 -. x1) *. (x2 -. x1)) +. ((y2 -. y1) *. (y2 -. y1)))

(* liczy punkt symetryczny wobec prostej 2 punktow *)
let symetria a p1 p2 =
  let dl = m (m p2 p1 (-.)) (odleglosc (0.0 , 0.0) (m p2 p1 (-.)) |> para) (/.) in
  let dw = m p1 (m dl ((skalarny dl (m a p1 (-.))) |> para) ( *.)) (+.) in
  m (m dw (2.0 |> para) ( *.)) a (-.) 

(* zwraca prostokat o wymiarach od punktu do punktu *)
let prostokat (x1, y1) (x2, y2) = fun p ->
  if ((fst p) +. eps >= x1)
  && ((fst p) -. eps <= x2)
  && ((snd p) +. eps >= y1)
  && ((snd p) -. eps <= y2)
  then 1 else 0                                      

(* zwraca kolko w srodku w pkt a i promieniu r *)
let kolko a r = fun p ->
  if (odleglosc a p -. eps <= r)
  then 1 else 0

(* iloczyn wektorowy sprawdza pozycje punktu wzgledem prostej dokonuje zlozenia funkcji - zwraca funkcje konsekwentnie *)
(*  "rozwijajaca" punkt przez kolejne symetrie i sprawdza czy jest w obrebie prostokata *)
let zloz a b k = fun p ->
  let pos = wektorowy (m a p (-.)) (m b p (-.)) in
  if abs_float pos <= eps then k p
  else if pos > 0.0 then k p + k (symetria p a b)
  else 0 

(* sklada kartke l.size razy *)
let skladaj l k =
  let fold_k acc p = zloz (fst p) (snd p) acc
  in List.fold_left fold_k k l
