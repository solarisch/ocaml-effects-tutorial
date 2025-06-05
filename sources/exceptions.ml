open Effect
open Effect.Deep

type _ Effect.t += Invalid_argument : exn -> _ Effect.t 

let raise (e : exn) : 'a = perform (Invalid_argument e)

let try_with (f : unit -> 'a) (h : exn -> 'a) : 'a = 
  try_with f () {effc = (fun (type c) (eff: c Effect.t) ->
    match eff with 
    | Invalid_argument e -> Some (fun _ -> h e)
    | _ -> None
  )} 

exception Invalid_argument

(** [sqrt f] returns the square root of [f].
    @raise Invalid_argument if f < 0. *)
let sqrt f =
  if f < 0.0 then raise Invalid_argument
  else sqrt f

let _ =
  try_with (fun () ->
    let r = sqrt 42.42 in
    Printf.printf "%f\n%!" r;
    let r = sqrt (-1.0) in
    Printf.printf "%f\n" r)
  (fun _ -> Printf.printf "Invalid_argument to sqrt\n")

(* Prints:
   6.513064
   Invalid_argument to sqrt *)
