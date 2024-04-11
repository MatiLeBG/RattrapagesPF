(* ordre : du moins au plus significatif; le *nombre* 0 est représenté par []; toute liste finit forcément par un One avant le [] *)
type digit = Zero | One
type t = digit list

let zero = []
let is_zero t = t = []

let rec inc =
  (* ---------- EXERCICE ---------- *)
(* DEBUT REPONSE *)
function 
  | [] -> [One]
  | Zero::q -> One::q
  | One::q -> Zero::(inc q)
(* FIN REPONSE *)

let rec dec =
  (* ---------- EXERCICE ---------- *)
(* DEBUT REPONSE *)
function 
  | [] -> []
  | One::q -> Zero::q
  | Zero::q -> One::(dec q)
(* FIN REPONSE *)

let to_int n =
  (* ---------- EXERCICE ---------- *)
(* DEBUT REPONSE *)
  let rec aux n i = match n with
    | [] -> 0
    | Zero::q -> aux q (i*2)
    | One::q -> i + (aux q (i*2))
  in
  aux n 1
(* FIN REPONSE *)

let rec add n1 n2 =
  (* ---------- EXERCICE ---------- *)
(* DEBUT REPONSE *)
match n1, n2 with 
  | _, [] -> n1
  | [], _ -> n2
  | Zero::q1, Zero::q2 -> Zero::(add q1 q2)
  | Zero::q1, One::q2 -> One::(add q1 q2)
  | One::q1, Zero::q2 -> One::(add q1 q2)
  | One::q1, One::q2 -> One::(inc (add q1 q2))

(* FIN REPONSE *)

let print_digit = function
  | Zero -> print_string "0 "
  | One -> print_string "1 "

let print (ds : t) : unit =
  print_string "[ ";
  List.iter print_digit ds;
  print_string " ]"

let of_int (n : int) : t =
  if n < 0 then failwith "of_int: paramètre négatif"
  else
    (* algorithme très naïf : on incrémente de façon répétée... *)
    let rec loop acc = function 0 -> acc | n -> loop (inc acc) (n - 1) in
    loop zero n
