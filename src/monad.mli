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

(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
type ('a, 'err, 'prio, 'state) t

(* Monadic boilerplate *)

val return : 'a -> ('a, 'err, 'prio, 'state) t

val bind :
     ('a -> ('b, 'err, 'prio, 'state) t)
  -> ('a, 'err, 'prio, 'state) t
  -> ('b, 'err, 'prio, 'state) t

val ( let* ) :
     ('a, 'err, 'prio, 'state) t
  -> ('a -> ('b, 'err, 'prio, 'state) t)
  -> ('b, 'err, 'prio, 'state) t

val map :
  ('a -> 'b) -> ('a, 'err, 'prio, 'state) t -> ('b, 'err, 'prio, 'state) t

val ( let+ ) :
  ('a, 'err, 'prio, 'state) t -> ('a -> 'b) -> ('b, 'err, 'prio, 'state) t

(* State *)

val map_state : ('state -> 'state) -> (unit, 'err, 'prio, 'state) t

val fold_state : ('state -> 'a) -> ('a, 'err, 'prio, 'state) t

(* Symbolic execution *)

val choose :
     ('a, 'err, 'prio, 'state) t
  -> ('a, 'err, 'prio, 'state) t
  -> ('a, 'err, 'prio, 'state) t

val fail : 'err -> ('a, 'err, 'prio, 'state) t

val fork :
     parent:('a, 'err, 'prio, 'state) t
  -> child:'prio * ('a, 'err, 'prio, 'state) t
  -> ('a, 'err, 'prio, 'state) t

val yield : 'prio -> (unit, 'err, 'prio, 'state) t

val run :
  ('a, 'err, 'prio, 'state) t -> 'state -> ('a, 'err, 'prio, 'state) schedulable
