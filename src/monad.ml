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

(** Add a notion of State to the Schedulable monad. "Transformer without module
    functor" style. *)
type ('a, 'err, 'prio, 'state) t =
  { run :
      'r.
         'state
      -> ('a -> 'state -> ('r, 'err, 'prio, 'state) schedulable)
      -> ('r, 'err, 'prio, 'state) schedulable
  }
[@@unboxed]

(* State monadic boilerplate *)

let[@inline] return x = { run = (fun state k -> k x state) }

let[@inline] bind f x =
  { run = (fun state k -> x.run state (fun v state -> (f v).run state k)) }

let[@inline] map f x =
  { run = (fun state k -> x.run state (fun v state -> k (f v) state)) }

let[@inline] ( let* ) x f = bind f x

let[@inline] ( let+ ) x f = map f x

(** State operations. *)

let[@inline] fold_state f = { run = (fun state k -> k (f state) state) }

let[@inline] map_state f = { run = (fun state k -> k () (f state)) }

(** Symbolic execution primitives. *)

(** Create two new branches, they do not yield so the yield should be created
    manually! *)
let[@inline] choose x y =
  { run = (fun state k -> Choice (x.run state k, y.run state k)) }

(** Yield the current branch (i.e. add it to the work queue so that it gets
    executed later. )*)
let[@inline] yield prio =
  { run = (fun state k -> Yield (prio, fun () -> k () state)) }

(** Child will be a new branch that immediately yields, and parent will execute
    directly without yielding. *)
let[@inline] fork ~parent ~child =
  let prio, child = child in
  let child =
    let* () = yield prio in
    child
  in
  choose parent child

let[@inline] fail err = { run = (fun _state _k -> Error err) }

let[@inline] run m state = m.run state (fun v state -> Ok (v, state))
