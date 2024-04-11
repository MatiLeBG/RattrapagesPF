(* ordre : du moins au plus significatif; le *nombre* 0 est représenté par []; toute liste finit forcément par un One avant le [] *)
type digit = Zero | One
type t = digit list

let zero = []
let is_zero t = t = []

let sept = [One; One; One]
let huit = [Zero; Zero; Zero; One]

let vingt_cinq = [One; Zero; Zero; One; One]
let vingt_six = [Zero; One; Zero; One; One]

let rec inc =
  fun n ->
    match n with
    | [] -> [One]
    | t::q -> if t == Zero then
                One::q
              else
                Zero::(inc q) 

let%test _ = inc [] = [One]
let%test _ = inc sept = huit
let%test _ = inc vingt_cinq = vingt_six



let rec dec =
  fun n ->
    match n with
    | [] -> []
    | [One] -> []
    | t::q -> if t == One then
                Zero::q
              else
                One::(dec q)

let%test _ = dec [One] = []
let%test _ = dec huit = sept
let%test _ = dec vingt_six = vingt_cinq


let rec to_int =
  (* ---------- EXERCICE ---------- *)
(* DEBUT REPONSE *)
fun n ->
  match n with
  | [] -> 0
  | [One] -> 1
  | One::q -> 1 + 2 * (to_int q)
  | Zero::q -> 2 * (to_int q)

(* FIN REPONSE *)

(*
let rec add n1 n2 =
  (* ---------- EXERCICE ---------- *)
(* DEBUT REPONSE *)
failwith "TODO"
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
*)