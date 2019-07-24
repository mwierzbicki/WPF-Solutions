(*****************************)
(* Autor: Marcin Wierzbicki. *)
(*****************************)

(* typ kolejki - albo Nil - pusta, albo Node - w postaci lewy_syn, wartosc, prawy_syn, wysokosc do Nil *)
type 'a queue = 
  | Nil
  | Node of 'a queue * 'a * 'a queue * int 

(* zwraca lewego syna kolejki *)
let left_son a = match a with
  | Nil -> Nil
  | Node(x , _ , _ , _) -> x
(* zwraca wartosc Node albo wyjatek jezeli sprawdzany jest Nil *)
let value a = match a with
  | Nil -> failwith "Checking value of Nil"
  | Node(_ , x , _ , _) -> x
(* zwraca prawego syna kolejki *)
let right_son a = match a with
  | Nil -> Nil
  | Node(_ , _ , x , _) -> x
(* zwraca wysokosc do Nil - jezeli sprawdzany jest Nil, wysokosc wynosi -1 ze wzgledu na implementacje *)
let height a = match a with
  | Nil -> -1
  | Node(_ , _ , _ , x) -> x

(* laczenie drzew zgodnie ze specyfikacja w zadaniu *)
let rec join a b =
  match a, b with
  | a, Nil -> a
  | Nil, b -> b
  | a, b ->
  (* dwie funkcje pomocnicze - zwracaja odpowiednio node z mniejsza i wieksza wartoscia *)
  let minval_node a b = if (value a <= value b) then a else b in
  let maxval_node a b = if (value a > value b) then a else b in
  (* analogicznie jak powyzej - zwracaja odpowiednio node z mniejsza i wieksza prawa wysokoscia *)
  let minheight_node a b = if (height a <= height b) then a else b in
  let maxheight_node a b = if (height a > height b) then a else b in
  (* join zgodnie z trescia zadania *)
  let d1 = minval_node a b in
  let d2 = maxval_node a b in
  let d3 = join (right_son d1) d2 in
  let newl = maxheight_node (left_son d1) d3 in
  let newr = minheight_node (left_son d1) d3 in 
  Node(newl , value d1 , newr , (min (height newl) (height newr)) + 1)

(* pusta kolejka *)
let empty = Nil

(* dodanie elementu - tworzenie pustej kolejki jednoelementowej i laczenie z dostarczona *)
let add e q = join q (Node(Nil , e , Nil, 0))

(* wyjatek - kolejka pusta *)
exception Empty

(* usuniecie elementu - zwraca element i kolejke *)
let delete_min q = 
  match q with
  | Nil -> raise Empty
  | Node (lq, e, rq, h) -> (e , (join lq rq))

(* test czy kolejka pusta *)
let is_empty q = (q = Nil)
