open CamomileLibrary

val draw : LTerm_draw.context -> UChar.t Zipper.t -> unit

val handle : View.Env.t -> key:LTerm_key.t -> UChar.t Zipper.t -> unit Lwt.t
