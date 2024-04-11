  module M (* : Ral_intf.S *) = struct
  type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
  type 'a digit = Zero | One of 'a tree
  type 'a t = 'a digit list

  exception Empty

  let empty = []

  (* à définir : cons, head, tail et get *)

  let rec cons d = function
    | [] -> [One (Leaf d)]
    | Zero::q -> One (Leaf d) :: q
    | One(t)::q -> Zero :: cons d q

  let rec uncons = function
    | [] -> failwith "Empty"
    | Zero::q -> let (x, t) = uncons q in (x, One t::q)
    | One (Leaf x)::q -> (x, Zero::q)
    | One (Node(_, r)) :: q -> (1, One r::q)
end
