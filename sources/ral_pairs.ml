module M (* : Ral_intf.S *) = struct
  type 'a t = Nil | Zero of ('a * 'a) t | One of 'a * ('a * 'a) t

  exception Empty

  let empty = Nil

(* A COMPLETER *)
end
