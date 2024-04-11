(* ordre : du moins au plus significatif; le *nombre* 0 est représenté par []; toute liste finit forcément par un One avant le [] *)
type digit = Zero | One
type t = digit list

let zero = []
let is_zero t = t = []

let un = [One]
let deux = [Zero ; One]
let sept = [One; One; One]
let huit = [Zero; Zero; Zero; One]
let quinze = [One; One; One; One]
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

let%test _ = inc zero = un
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
fun n ->
  match n with
  | [] -> 0
  | [One] -> 1
  | t::q -> if t == One then
              1 + 2 * (to_int q)
            else
              2 * (to_int q)

let%test _ = to_int zero = 0
let%test _ = to_int un = 1
let%test _ = to_int sept = 7
let%test _ = to_int huit = 8
let%test _ = to_int vingt_cinq = 25
let%test _ = to_int vingt_six = 26
              


let rec add n1 n2 =
match n1, n2 with
| ([],_) -> n2
| (_,[]) -> n1
| ([One], _) -> inc n2
| (_,[One]) -> inc n1
| (t1::q1, t2::q2) -> match t1, t2 with
                      | (Zero, Zero) -> Zero::(add q1 q2)
                      | (One, One) -> Zero::(inc (add q1 q2))
                      | _ -> One::(add q1 q2)
            
let%test _ = add zero zero = zero
let%test _ = add zero un = un
let%test _ = add un un = deux
let%test _ = add sept huit = quinze

(*

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