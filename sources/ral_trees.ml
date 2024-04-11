  module M (* : Ral_intf.S *) = struct
  type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
  type 'a digit = Zero | One of 'a tree
  type 'a t = 'a digit list

  exception Empty

  let empty = []

  (* à définir : cons, head, tail et get *)

(* A COMPLETER *)
end
