(** Reactive signals. *)

type 'a t
(** The type for signals that emit values of type ['a]. *)

(** {2 Creating signals} *)

val make : ?equal:('a -> 'a -> bool) -> ?label:string -> 'a -> 'a t
(** [make x] is a signal with an initial value [x]. *)

val label : 'a t -> string

val reducer : ('acc -> 'a -> 'acc) -> 'acc -> 'acc t * ('a -> unit)
(** [reducer f init] is [(signal, dispatch)], that is, a reducer signal and a dispatch function. The
    signal starts with the initial accumulator value [init]. Whenever [dispatch] is called with a
    value, the accumulator is updated using [f] and emitted to [signal]. *)

val null : unit t
(** [null] is a constant signal of [()]. It ignores it's subscribers and ignores values emitted to
    it. *)

(** {2 Notifications}

    Notifications control the propagation of signal values to subscribers. The value emitting
    functions such as {!val:emit} and {!val:update} accept an optional [~notify] argument
    which changes the propagation strategy. For example, it is possible to fully prevent notifying
    the subscribers by using [Signal.emit ~notify:Signal.never] and later call {!val:Signal.trigger}
    to notify the subscribers at a more appropriate time. *)

type notification
(** The type for notifications. *)

val never : notification
(** An notification that never notifies the subscribers. *)

val now : notification
(** A notification that notifies subscribers immediately. *)

val scope : (notification -> unit) -> unit
(** [scope f] calls [f] with a notification that can be used to batch notify all subscribers for
    emits that happened in [f]'s scope. The current values of the signals are still changed immediately. *)


(** {2 Updating, getting and propagating values} *)

val emit : ?notify:notification -> 'a -> 'a t -> unit
(** [emit ?notify x s] changes the value of signal [s] to [x]. The emitted value will be dispatched to all
    subscribers of [s] according to [notify] (defaults to {!val:now}). *)

val set : 'a -> 'a t -> unit
(** [set x s] is [emit ~notify:never x s], that is, using [set] changes the
    current value of the signal [s], but will {e not} notify the subscribers. *)

val update : ?notify:notification -> ('a -> 'a) -> 'a t -> unit
(** [update ?step f s] changes the current value of [s] using [f], emitting the result. *)

val get : 'a t -> 'a
(** [get s] is the {e current} value of signal [s]. *)

val trigger : 'a t -> unit
(** Emits the current value of the signal to all subscribers. *)


(** {2 Subscribing to values} *)

type sub = unit -> unit
(** The type for subscription handlers. Call to unsubscribe. *)

val use : ('a -> unit) -> 'a t -> unit
(** [use f s] permanently subscribes to [s] and immediately calls the callback [f] with the current
    value of the signal. *)

val use' : ?label:string -> ('a -> unit) -> 'a t -> sub
(** [use' f s] subscribes to [s] and immediately calls the callback [f] with the current
    value of the signal. Returns a subscription handle that can be used to unsubscribe [f] from the
    signal [s]. *)

val use2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [use2 f s1 s2] permanently subscribes to [s1] and [s2] and immediately calls the callback [f]
    with the current values of the signals. *)

val sub : ('a -> unit) -> 'a t -> unit
(** [sub f s] permanently subscribes to [s] without immediately calling [f], instead [f] is called
    when new values are emitted to [s]. *)

val sub' : ('a -> unit) -> 'a t -> sub
(** [sub' f s] subscribes to [s] without immediately calling [f], instead [f] is called
    when new values are emitted to [s]. Returns a subscription handle that can be used to
    unsubscribe [f] from the signal [s]. *)

val sub2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [sub2 f s1 s2] permanently subscribes to [s1] and [s2] without immediately calling [f], instead
    [f] is called when new values are emitted to [s1] or [s2]. *)

val unsub : sub -> unit
(** Unsubscribe from a subscription. *)

(** {2 Transforming signals} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] is a signal derived from [s] with the values mapped using [f].

    Note that the resulting signal can be subsribed to independently from [s]. *)

val map2 : ('a -> 'b -> 'r) -> 'a t -> 'b t -> 'r t
(** [map f s1 s2] is a signal that combines the values from [s1] and [s2] using [f]. *)

val map3 : ('a -> 'b -> 'c -> 'r) -> 'a t -> 'b t -> 'c t -> 'r t
(** [map f s1 s2 s3] is a signal that combines the values from [s1], [s2] and [s3] using [f]. *)

val const : 'a -> _ t -> 'a t
(** [const x s] is a constant signal transformer for [s]. That is, it will {e always} emits [x]
    whenever values are emitted to [s]. *)

val tap : ('a -> unit) -> 'a t -> 'a t
(** [tap f s] intercepts all values emitted by signal [s] and calls the effectful function [f] with
    the value. *)

val filter : ('a -> bool) -> seed:'a -> 'a t -> 'a t
(** [filter pred ~seed s] is a signal derived from [s] that only emits values that satisfy the
    predicate [pred]. If the current value of [s] does not satisfy [pred], the value of the filtered
    signal is set to [seed].

    Note that the resulting signal can be subsribed to independently from [s]. *)

val filter_map : ('a -> 'b option) -> seed:'b -> 'a t -> 'b t
(** [filter_map f ~seed s] is a signal derived from [s] that emits values transformed with [f] that
    are not [None]. If the current value of [s] results in [None], the value of the signal is set to
    [seed].

    Note that the resulting signal can be subsribed to independently from [s]. *)

val reduce : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc t
(** [reduce f init s] is a signal computed from continously updating [init] using a reducing
    function [f] applied to [init] and emitted values from [s].

    Emitting values directly to this signal will reset the internal accumulator. *)

val uniq : ?equal:('a -> 'a -> bool) -> 'a t -> 'a t
(** [uniq ?equal s] is a signal that prevents emitting values to subscribers that are considered
    equal according to [equal]. By default physical equality is used for [equal]. *)

(** {2 Combining signals} *)

val select : 'a t list -> 'a t
(** [select l] is a signal that selects values emitted by all signals in list [l].

    {b Raises}: [Invalid_argument] if [l] is empty. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair s1 s2] is a signal that combines the values from [s1] and [s2] and emits them as pairs. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple s1 s2 s3] is a signal that combines the values from [s1], [s2] and [s3] and emits them
    as triple. *)

val t5 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t
val t6 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t

val apply : ('a -> 'b) t -> 'a t -> 'b t
(** [apply f_s x_s] is a signal produced from applying values from signal [x_s] to the functions in
    signal [f_s]. *)

val sample : ?equal:('b -> 'b -> bool) -> on:_ t -> 'b t -> 'b t
(** [sample ?equal ~on:s1 s2] samples values from [s2] any time a value is emitted to [s1]. *)

(* val forward : 'a t -> 'a t -> unit *)
(** [forward s1 s2] forwards all values emitted by [s1] to [s2]. *)

(* {2 Syntax definitions} *)

module Syntax : sig
  (** Syntax for working with signal values.

      {[
        open Signal.Syntax

        let sum_signal s1 s2 =
          let+ x1 = s1 and+ x2 = s2 in
          x1 + x2
      ]} *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( <~ ) : ('a -> 'b) -> 'a t -> 'b t
  val ( ~~ ) : ('a -> 'b) t -> 'a t -> 'b t
end
