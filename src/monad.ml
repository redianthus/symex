(** A monad representing computation that can be cooperatively scheduled.
    Computations can yield ([Yield]), and split into two non deterministic
    choices ([Choice]). They can also fail/stop ([Ok] versus [Error]). *)

type ('a, 'err, 'prio, 'state) schedulable =
  | Ok of 'a * 'state
  | Error of 'err
  | Yield of 'prio * (unit -> ('a, 'err, 'prio, 'state) schedulable)
  | Choice of
      ('a, 'err, 'prio, 'state) schedulable
      * ('a, 'err, 'prio, 'state) schedulable

(* Schedulable monadic boilerplate *)

let[@inline] rec bind_schedulable f = function
  | Ok (v, state) -> f v state
  | Error e -> Error e
  | Yield (prio, step) -> Yield (prio, fun () -> bind_schedulable f (step ()))
  | Choice (a, b) -> Choice (bind_schedulable f a, bind_schedulable f b)

let[@inline] rec map_schedulable f = function
  | Ok (v, state) -> Ok (f v, state)
  | Error e -> Error e
  | Yield (prio, step) -> Yield (prio, fun () -> map_schedulable f (step ()))
  | Choice (a, b) -> Choice (map_schedulable f a, map_schedulable f b)

(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
type ('a, 'err, 'prio, 'state) t =
  'state -> ('a, 'err, 'prio, 'state) schedulable

(* State monadic boilerplate *)

let[@inline] bind f x = fun state -> bind_schedulable f (x state)

let[@inline] map f x = fun state -> map_schedulable f (x state)

let[@inline] return x = fun state -> Ok (x, state)

let[@inline] ( let* ) x f = bind f x

let[@inline] ( let+ ) x f = map f x

(** State operations. *)

let[@inline] fold_state f = fun state -> Ok (f state, state)

let[@inline] map_state f = fun state -> Ok ((), f state)

(** Symbolic execution primitives. *)

(* Create two new branches, they do not yield so the yield should be created manually! *)
let[@inline] choose x y = fun state -> Choice (x state, y state)
(* Yield the current branch (i.e. add it to the work queue so that it gets executed later. )*)

let[@inline] yield prio = fun state -> Yield (prio, fun () -> Ok ((), state))

(* Child will be a new branch that immediately yields, and parent will execute directly without yielding. *)
let[@inline] fork ~parent ~child =
  let prio, child = child in
  let child = bind (fun () -> child) (yield prio) in
  choose parent child

let[@inline] fail err = fun _state -> Error err

let[@inline] run f state = f state
