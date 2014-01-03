type 'a or_error = ('a, string) Core.Std.Result.t Lwt.t

type uri = string

module Field : sig
  type t = [ `album | `artist | `track ]
  type with_extra = [ t | `any ]
end

val search :
  ?dest:Lwt_unix.sockaddr ->
  ?tag:[< Field.with_extra ] ->
  string ->
  Types.SearchResult.t list or_error
(** Default value for [dest] is localhost, default [tag] is [`any]. *)

val play : ?dest:Lwt_unix.sockaddr -> uri -> unit or_error

val queue : ?dest:Lwt_unix.sockaddr -> uri -> unit or_error

val get_album : ?dest:Lwt_unix.sockaddr -> uri -> string -> Types.Track.t list or_error

val get_artist : ?dest:Lwt_unix.sockaddr -> uri -> string -> Types.Album.t list or_error
