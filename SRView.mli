val draw : LTerm_draw.context -> View.SearchResult.state -> unit

val handle : View.Env.t -> key:LTerm_key.t -> View.SearchResult.state -> unit Lwt.t
