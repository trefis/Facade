open Types

val draw : LTerm_draw.context -> View.sr_state -> unit

val handle : Env.t -> key:LTerm_key.t -> View.sr_state -> unit Lwt.t
