(** A monad representing computation that can be cooperatively scheduled. A
    computation can stop ([Prune]). Computations can yield ([Yield]), and split
    into two non deterministic choices ([Choice]). They can also fail ([Ok]
    versus [Error]). *)
module Schedulable = struct
  type ('a, 'err, 'prio) t =
    | Prune
    | Ok of 'a
    | Error of 'err
    | Yield of 'prio * (unit -> ('a, 'err, 'prio) t)
    | Choice of ('a, 'err, 'prio) t * ('a, 'err, 'prio) t

  let rec bind (x : ('a, 'err, 'prio) t) (f : 'a -> ('b, 'err, 'prio) t) :
    ('b, 'err, 'prio) t =
    match x with
    | Prune -> Prune
    | Ok v -> f v
    | Error e -> Error e
    | Yield (prio, step) -> Yield (prio, fun () -> bind (step ()) f)
    | Choice (a, b) -> Choice (bind a f, bind b f)

  let[@inline] rec map (f : 'a -> 'b) (x : ('a, 'err, 'prio) t) :
    ('b, 'err, 'prio) t =
    match x with
    | Prune -> Prune
    | Ok v -> Ok (f v)
    | Error e -> Error e
    | Yield (prio, step) -> Yield (prio, fun () -> map f (step ()))
    | Choice (a, b) -> Choice (map f a, map f b)
end

(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
type ('a, 'err, 'prio, 'state) t =
  'state -> ('a * 'state, 'err, 'prio) Schedulable.t

let[@inline] return (x : 'a) : ('a, _, _, 'state) t =
 fun (state : 'state) -> Schedulable.Ok (x, state)

let[@inline] bind (x : ('a, 'err, 'prio, 'state) t)
  (f : 'a -> ('b, 'err, 'prio, 'state) t) : ('b, 'err, 'prio, 'state) t =
 fun (state : 'state) ->
  Schedulable.bind (x state) (fun (x, state) -> f x state)

let[@inline] ( let* ) (x : ('a, 'err, 'prio, 'state) t) f : _ t = bind x f

let[@inline] map (f : 'a -> 'b) (x : ('a, 'err, 'prio, 'state) t) :
  ('b, 'err, 'prio, 'state) t =
 fun (state : 'state) ->
  Schedulable.map (fun (x, state) -> (f x, state)) (x state)

let[@inline] ( let+ ) (x : ('a, 'err, 'prio, 'state) t) (f : 'a -> 'b) :
  ('b, 'err, 'prio, 'state) t =
  map f x

(** State operations. *)

let[@inline] fold_state (f : 'state -> 'a) : ('a, 'err, 'prio, 'state) t =
 fun (state : 'state) -> Schedulable.Ok (f state, state)

let[@inline] map_state (f : 'state -> 'state) : (unit, 'err, 'prio, 'state) t =
 fun (state : 'state) -> Schedulable.Ok ((), f state)

(** Symbolic execution primitives. *)

(* Create two new branches, they do not yield so the yield should be created manually! *)
let[@inline] choose (x : ('a, 'err, 'prio, 'state) t)
  (y : ('a, 'err, 'prio, 'state) t) : ('a, 'err, 'prio, 'state) t =
 fun state -> Schedulable.Choice (x state, y state)

(* Yield the current branch (i.e. add it to the work queue so that it gets executed later. )*)
let[@inline] yield (prio : 'prio) : (unit, 'err, 'prio, 'state) t =
 fun (state : 'state) ->
  Schedulable.Yield (prio, fun () -> Schedulable.Ok ((), state))

(* Child will be a new branch that immediately yields, and parent will execute directly without yielding. *)
let[@inline] fork ~(parent : ('a, 'err, 'prio, 'state) t)
  ~(child : 'prio * ('a, 'err, 'prio, 'state) t) : ('a, 'err, 'prio, 'state) t =
  let prio, child = child in
  let child = bind (yield prio) (fun () -> child) in
  choose parent child

let[@inline] prune () : ('a, 'err, 'prio, 'state) t =
 fun (_state : 'state) -> Schedulable.Prune

let[@inline] fail (err : 'err) : ('a, 'err, 'prio, 'state) t =
 fun (_state : 'state) -> Error err

let[@inline] run (f : ('a, 'err, 'prio, 'state) t) (state : 'state) :
  ('a * 'state, 'err, 'prio) Schedulable.t =
  f state
