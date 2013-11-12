type 'a t

val empty : 'a t
val singleton : 'a -> 'a t

val current : 'a t -> 'a
val set_current : 'a t -> 'a -> 'a t

val insert : 'a t -> 'a -> [ `before | `after ] -> 'a t
val delete : 'a t -> [ `before | `after ] -> 'a t

val forward  : 'a t -> 'a t
val backward : 'a t -> 'a t

val drop_tail : 'a t -> 'a t

val to_list : 'a t -> 'a list

val fold : 'a t -> init:'b -> f:(bool -> 'b -> 'a -> 'b) -> 'b
val iter : 'a t -> f:(bool -> 'a -> unit) -> unit
