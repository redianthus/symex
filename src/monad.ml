(** A monad representing computation that can be cooperatively scheduled. A
    computation can stop ([Prune]). Computations can yield ([Yield]), and split
    into two non deterministic choices ([Choice]). They can also fail ([Ok]
    versus [Error]). *)

type ('a, 'err, 'prio, 'state) schedulable =
  | Prune
  | Ok of 'a * 'state
  | Error of 'err
  | Yield of 'prio * (unit -> ('a, 'err, 'prio, 'state) schedulable)
  | Choice of
      ('a, 'err, 'prio, 'state) schedulable
      * ('a, 'err, 'prio, 'state) schedulable

(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
type ('a, 'err, 'prio, 'state) t =
  'state -> ('a, 'err, 'prio, 'state) schedulable

(* Schedulable+State monadic boilerplate *)

let[@inline] rec bind_schedulable (x : ('a, 'err, 'prio, 'state) schedulable)
  (f : 'a -> 'state -> ('b, 'err, 'prio, 'state) schedulable) :
  ('b, 'err, 'prio, 'state) schedulable =
  match x with
  | Prune -> Prune
  | Ok (v, state) -> f v state
  | Error e -> Error e
  | Yield (prio, step) -> Yield (prio, fun () -> bind_schedulable (step ()) f)
  | Choice (a, b) -> Choice (bind_schedulable a f, bind_schedulable b f)

let[@inline] bind (x : ('a, 'err, 'prio, 'state) t)
  (f : 'a -> ('b, 'err, 'prio, 'state) t) : ('b, 'err, 'prio, 'state) t =
 fun (state : 'state) -> bind_schedulable (x state) f

let[@inline] rec map_schedulable (f : 'a -> 'b)
  (x : ('a, 'err, 'prio, 'state) schedulable) :
  ('b, 'err, 'prio, 'state) schedulable =
  match x with
  | Prune -> Prune
  | Ok (v, state) -> Ok (f v, state)
  | Error e -> Error e
  | Yield (prio, step) -> Yield (prio, fun () -> map_schedulable f (step ()))
  | Choice (a, b) -> Choice (map_schedulable f a, map_schedulable f b)

let[@inline] map (f : 'a -> 'b) (x : ('a, 'err, 'prio, 'state) t) :
  ('b, 'err, 'prio, 'state) t =
 fun (state : 'state) -> map_schedulable f (x state)

(* State monadic boilerplate *)

let[@inline] return (x : 'a) : ('a, _, _, 'state) t =
 fun (state : 'state) -> Ok (x, state)

let[@inline] ( let* ) (x : ('a, 'err, 'prio, 'state) t) f : _ t = bind x f

let[@inline] ( let+ ) (x : ('a, 'err, 'prio, 'state) t) (f : 'a -> 'b) :
  ('b, 'err, 'prio, 'state) t =
  map f x

(** State operations. *)

let[@inline] fold_state (f : 'state -> 'a) : ('a, 'err, 'prio, 'state) t =
 fun (state : 'state) -> Ok (f state, state)

let[@inline] map_state (f : 'state -> 'state) : (unit, 'err, 'prio, 'state) t =
 fun (state : 'state) -> Ok ((), f state)

(** Symbolic execution primitives. *)

(* Create two new branches, they do not yield so the yield should be created manually! *)
let[@inline] choose (x : ('a, 'err, 'prio, 'state) t)
  (y : ('a, 'err, 'prio, 'state) t) : ('a, 'err, 'prio, 'state) t =
 fun state -> Choice (x state, y state)

(* Yield the current branch (i.e. add it to the work queue so that it gets executed later. )*)
let[@inline] yield (prio : 'prio) : (unit, 'err, 'prio, 'state) t =
 fun (state : 'state) -> Yield (prio, fun () -> Ok ((), state))

(* Child will be a new branch that immediately yields, and parent will execute directly without yielding. *)
let[@inline] fork ~(parent : ('a, 'err, 'prio, 'state) t)
  ~(child : 'prio * ('a, 'err, 'prio, 'state) t) : ('a, 'err, 'prio, 'state) t =
  let prio, child = child in
  let child = bind (yield prio) (fun () -> child) in
  choose parent child

let[@inline] prune () : ('a, 'err, 'prio, 'state) t =
 fun (_state : 'state) -> Prune

let[@inline] fail (err : 'err) : ('a, 'err, 'prio, 'state) t =
 fun (_state : 'state) -> Error err

let[@inline] run (f : ('a, 'err, 'prio, 'state) t) (state : 'state) :
  ('a, 'err, 'prio, 'state) schedulable =
  f state
