module List_ext = struct
  let mem ?(eq = Stdlib.( = )) x l =
    let rec search eq x l =
      match l with
      | [] -> false
      | y :: l' -> eq x y || search eq x l'
    in
    search eq x l

  let remove_one ~eq x l =
    let rec remove_one ~eq x acc l =
      match l with
      | [] -> assert false
      | y :: tl when eq x y -> y, List.rev_append acc tl
      | y :: tl -> remove_one ~eq x (y :: acc) tl
    in
    if mem ~eq x l then remove_one ~eq x [] l else invalid_arg "could not remove, element is not a member"
end

(* physical equality *)
let default_equal = Stdlib.(==)

(* Subscription registry for signals. *)
module Subs : sig
  type 'a sub = 'a -> unit
  type 'a t

  val empty : unit -> 'a t
  (* val length : 'a t -> int *)
  val add : ?label:string -> 'a sub -> 'a t -> unit
  val dispatch : 'a -> 'a t -> unit
  val remove : 'a sub -> 'a t -> unit
  (* val names : 'a t -> string list *)
end = struct
  type 'a sub = 'a -> unit
  type 'a t = (string * 'a sub ref) list ref

  let empty () = ref []
  (* let length subs = List.length !subs *)
  let add ?(label="") (k : 'a sub) subs = subs := List.append !subs [ (label, ref k) ]
  let dispatch x subs = List.iter (fun (_, k) -> !k x) !subs
  let remove k subs =
    let (_, k'), subs' = List_ext.remove_one ~eq:(fun (_, s1) (_, s2) -> !s1 == !s2) ("", ref k) !subs in
    (* If remove is called during dispatch, dispatch will have derefed the original list. We set the
       removed k to ignore to avoid calling it. *)
    k' := ignore;
    subs := subs'
  
  (* let names subs = List.map fst !subs *)
end

(* Notification *)

type notification = {
  add : (unit -> unit) -> unit;
  run : unit -> unit;
}

module Notification = struct
  let make () : notification =
    let fs = ref [] in
    let add f = fs := f :: !fs in
    let run () = List.iter (fun f -> f ()) (List.rev !fs) in
    { add; run }

  let now =
    let add f = f () in
    { add; run = ignore }

  let never = { add = ignore; run = ignore }
end

let never = Notification.never
let now = Notification.now

let scope f =
  let notification = Notification.make () in
  f notification;
  notification.run ()


(* Signal *)

type sub = unit -> unit

type !'a t = {
  name : string;
  mutable value : 'a;
  emit : ?notify:notification -> 'a -> unit;
  sub : ?label:string -> ('a -> unit) -> sub;
}

let gen_id = let i = ref (-1) in fun () -> incr i; string_of_int !i

let base ?(equal = default_equal) ~name value =
  let name = name ^ "#" ^ gen_id() in
  let subs = Subs.empty () in
  let sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs;
  in
  let rec s = { name; value; emit; sub }
  and emit ?(notify=now) x =
    if not (equal s.value x)then (
      s.value <- x;
      notify.add (fun () -> Subs.dispatch x subs)
    )
  in
  s

let make ?equal ?(label="make") value = base ?equal ~name:label value

let label s = s.name

let null =
  let sub ?label:_ _ () = () in
  let rec s = { name = "null"; value = (); emit; sub } and emit ?notify:_ _x = () in
  s

let get s = s.value
let emit ?notify x s = s.emit ?notify x
let set x s = emit ~notify:never x s
let update ?notify f s = s.emit ?notify (f s.value)
let trigger s = s.emit s.value
let sub k s = let _ : sub = (s.sub k) in ()
let sub' k s = s.sub k

let sub2 k s1 s2 =
  let _ : sub = s1.sub (fun x1 -> k x1 s2.value) in
  let _ : sub = s2.sub (fun x2 -> k s1.value x2) in
  ()

let use k s =
  let _ : sub = s.sub k in
  k s.value

let use' ?label k s =
  let unsub : sub = s.sub ?label k in
  k s.value;
  unsub

let use2 k s1 s2 =
  let _ : sub = s1.sub (fun x1 -> k x1 s2.value) in
  let _ : sub = s2.sub (fun x2 -> k s1.value x2) in
  k s1.value s2.value

let unsub sub = sub ()

let map f s =
  let s' = base ~name:"map" (f s.value) in
  let _ : sub = s.sub (fun x -> s'.emit (f x)) in
  s'

let const x s =
  let s' = base ~name:"const" x in
  let _ : sub = s.sub (fun _ -> s'.emit x) in
  s'

let tap f s =
  let s' = base ~name:"tap" s.value in
  let _ : sub =
    s.sub (fun x ->
        f x;
        s'.emit x
    )
  in
  s'

let pair s1 s2 =
  let subs = Subs.empty () in
  let rec s' = { name = "pair"; value = (s1.value, s2.value); emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = s1.sub (fun x1 -> emit (x1, s2.value)) in
  let _ : sub = s2.sub (fun x2 -> emit (s1.value, x2)) in
  s'

let triple s1 s2 s3 =
  let subs = Subs.empty () in
  let rec s' = { name = "triple"; value = (s1.value, s2.value, s3.value); emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = s1.sub (fun x1 -> emit (x1, s2.value, s3.value)) in
  let _ : sub = s2.sub (fun x2 -> emit (s1.value, x2, s3.value)) in
  let _ : sub = s3.sub (fun x3 -> emit (s1.value, s2.value, x3)) in
  s'

let t5 s1 s2 s3 s4 s5 =
  let subs = Subs.empty () in
  let rec s' = { name = "t5"; value = (s1.value, s2.value, s3.value, s4.value, s5.value); emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = s1.sub (fun x1 -> emit (x1, s2.value, s3.value, s4.value, s5.value)) in
  let _ : sub = s2.sub (fun x2 -> emit (s1.value, x2, s3.value, s4.value, s5.value)) in
  let _ : sub = s3.sub (fun x3 -> emit (s1.value, s2.value, x3, s4.value, s5.value)) in
  let _ : sub = s4.sub (fun x4 -> emit (s1.value, s2.value, s3.value, x4, s5.value)) in
  let _ : sub = s5.sub (fun x5 -> emit (s1.value, s2.value, s3.value, s4.value, x5)) in
  s'

let t6 s1 s2 s3 s4 s5 s6 =
  let subs = Subs.empty () in
  let rec s' = { name = "t6"; value = (s1.value, s2.value, s3.value, s4.value, s5.value, s6.value); emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = s1.sub (fun x1 -> emit (x1, s2.value, s3.value, s4.value, s5.value, s6.value)) in
  let _ : sub = s2.sub (fun x2 -> emit (s1.value, x2, s3.value, s4.value, s5.value, s6.value)) in
  let _ : sub = s3.sub (fun x3 -> emit (s1.value, s2.value, x3, s4.value, s5.value, s6.value)) in
  let _ : sub = s4.sub (fun x4 -> emit (s1.value, s2.value, s3.value, x4, s5.value, s6.value)) in
  let _ : sub = s5.sub (fun x5 -> emit (s1.value, s2.value, s3.value, s4.value, x5, s6.value)) in
  let _ : sub = s6.sub (fun x6 -> emit (s1.value, s2.value, s3.value, s4.value, s5.value, x6)) in
  s'

let filter pred ~seed s =
  let subs = Subs.empty () in
  let sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let rec s' = { name = "filter"; value = (if pred s.value then s.value else seed); emit; sub }
  and emit ?(notify=Notification.now) x =
    if pred x then (
      s'.value <- x;
      notify.add (fun () -> Subs.dispatch x subs)
    )
  in
  let _ : sub = s.sub emit in
  s'

let filter_map f ~seed s =
  let subs = Subs.empty () in
  let sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let rec s' =
    {
      name = "filter_map";
      value =
        ( match f s.value with
        | Some x -> x
        | None -> seed
        );
      emit;
      sub;
    }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  in
  let _ : sub =
    s.sub (fun x ->
        match f x with
        | Some x' -> emit x'
        | None -> ()
    )
  in
  s'

let reduce f init s =
  let subs = Subs.empty () in
  let sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let rec s' = { name = "reduce"; value = f init s.value; emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  in
  let _ : sub = s.sub (fun value -> s'.emit (f s'.value value)) in
  s'

let reducer f init =
  let events = base ~name:"reducer" None in
  let state_signal =
    reduce
      (fun state event_opt ->
        match event_opt with
        | Some event -> f state event
        | None -> state
      )
      init events
  in
  let dispatch event = emit (Some event) events in
  (state_signal, dispatch)

let select l =
  match l with
  | [] -> invalid_arg "Signal.select: empty signal list"
  | s1 :: _ ->
    let subs = Subs.empty () in
    let sub ?label k =
      Subs.add ?label k subs;
      fun () -> Subs.remove k subs
    in
    let rec s' = { name = "select"; value = s1.value; emit; sub }
    and emit ?(notify=Notification.now) x =
      s'.value <- x;
      notify.add (fun () -> Subs.dispatch x subs)
    in
    List.iter (fun s -> let _ : sub = s.sub emit in ()) l;
    s'

let uniq ?(equal = ( == )) s =
  let subs = Subs.empty () in
  let rec s' = { name = "uniq"; value = s.value; emit; sub }
  and emit ?(notify=Notification.now) x =
    if not (equal x s'.value) then (
      s'.value <- x;
      notify.add (fun () -> Subs.dispatch x subs)
    )
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = s.sub emit in
  s'

let map2 f s1 s2 =
  let subs = Subs.empty () in
  let rec s' = { name = "map2"; value = f s1.value s2.value; emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = (s1.sub (fun x1 -> emit (f x1 s2.value))) in
  let _ : sub = (s2.sub (fun x2 -> emit (f s1.value x2))) in
  s'

let map3 f s1 s2 s3 =
  let subs = Subs.empty () in
  let rec s' = { name = "map3"; value = f s1.value s2.value s3.value; emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = (s1.sub (fun x1 -> emit (f x1 s2.value s3.value))) in
  let _ : sub = (s2.sub (fun x2 -> emit (f s1.value x2 s3.value))) in
  let _ : sub = (s3.sub (fun x3 -> emit (f s1.value s2.value x3))) in
  s'

let sample ?equal ~on:s1 s2 =
  let s' = base ?equal ~name:"sample" s2.value in
  let _ : sub = (s1.sub (fun _ -> s'.emit s2.value)) in
  s'

let apply f_s x_s =
  let subs = Subs.empty () in
  let rec s' = { name = "apply"; value = f_s.value x_s.value; emit; sub }
  and emit ?(notify=Notification.now) x =
    s'.value <- x;
    notify.add (fun () -> Subs.dispatch x subs)
  and sub ?label k =
    Subs.add ?label k subs;
    fun () -> Subs.remove k subs
  in
  let _ : sub = (f_s.sub (fun f -> emit (f x_s.value))) in
  let _ : sub = (x_s.sub (fun x -> emit (f_s.value x))) in
  s'

(* let forward s1 s2 =
  sub (fun x -> emit x s2)  *)



module Syntax = struct
  let ( let+ ) s f = map f s
  let ( and+ ) = pair
  let ( <~ ) = map
  let ( ~~ ) = apply
end
