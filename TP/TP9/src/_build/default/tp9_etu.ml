module GreenThreads =

  struct
    
    (* à compléter/modifier *)
    type res = 
    | Fork of ((unit -> unit) * (unit->res)) (* premier element future process et deuxieme process à reprendre *)
    | Yield of (unit -> res) (* process à reprendre à un endroit particulier *)
    | Done;;


    let a () =

      let prompt0 = Delimcc.new_prompt() in
      

      let scheduler proc_init = 
        (*queue de processus : queue de (unit->res)*)
        let queue = Queue.create() in
        let rec handle result =
          match result with
          | Done -> if Queue.is_empty queue then 
                      () 
                    else 
                      let k = Queue.pop queue in handle(k ()) (* Exécuter le processus suivant de la file *)

          (* Note - let _= push k q in blabla <=> push k q ; blabla - L'opérateur ; est binaire super étrange *)
          | Yield k -> (Queue.push k queue) ; let k' =  Queue.pop queue in handle(k' ()) (* Réenfiler le processus reçu (avec son état k actuel) et exécuter le processus suivant de la file *)
          | Fork (prog, k) -> Queue.push k queue ; run prog (* Réenfiler le processus reçu (avec son état k actuel) et démarrer un nouveau programme *)
        
        and run prog = 
          (* run prend un programme et le transforme en process*)
          handle (Delimcc.push_prompt prompt0 (fun() -> prog() ; Done))
        in 
          run proc_init
      in
      
      let yield () = Delimcc.shift prompt0 (fun k -> Yield k) in
      let fork proc = Delimcc.shift prompt0 (fun k -> Fork(proc,k)) in
      let exit () = Delimcc.shift prompt0(fun _ -> Done) in

      let rec ping n =
        if n = 0 then exit()
        else (print_string "ping\n"; yield(); ping(n-1)) in
      
      
      let rec pong n =
          if n = 0 then exit()
          else (print_string "pong\n"; yield(); pong(n-1)) in
        
      fork (ping 5); fork (pong 5); exit()
    end

module type Channel =
  sig
    val create : unit -> ('a -> unit) * (unit -> 'a)
  end

module GTChannel : Channel =
  struct
    (* à compléter/modifier *)
    let create () = assert false;;
  end
    
(*
let sieve () =
  let rec filter reader =
    GreenThreads.(
      let v0 = reader () in
      if v0 = -1 then exit () else
      Format.printf "%d@." v0;
      yield ();
      let (writer', reader') = GTChannel.create () in
      fork (fun () -> filter reader');
      while true
      do
        let v = reader () in
        yield ();
        if v mod v0 <> 0 then writer' v;
        if v = -1 then exit ()
      done
    ) in
  let main () =
    GreenThreads.(
      let (writer, reader) = GTChannel.create () in
      fork (fun () -> filter reader);
      for i = 2 to 1000
      do
        writer i;
        yield ()
      done;
      writer (-1);
      exit ()
    ) in
  GreenThreads.scheduler main;;

  *)


let b () =
  GreenThreads.a()
