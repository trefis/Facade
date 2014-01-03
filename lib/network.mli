type 'a or_error = ('a, string) Core.Std.Result.t Lwt.t

type uri = string

module Artist : sig
  type t = { uri : uri ; name : string }
end

module Album : sig
  type t  = { uri : uri ; name : string ; artists : Artist.t list }
end

module Track : sig
  type t = {
    uri     : uri ;
    name    : string ;
    artists : Artist.t list ;
    album  : Album.t ;
  }
end

module SearchResult : sig
  type t = {
    source  : string ;
    artists : Artist.t list ;
    albums  : Album.t list ;
    tracks  : Track.t list ;
  }
end

module Field : sig
  type t = [ `album | `artist | `track ]
  type with_extra = [ t | `any ]
end

val search :
  ?dest:Lwt_unix.sockaddr ->
  ?tag:[< Field.with_extra ] ->
  string ->
  SearchResult.t list or_error
(** Default value for [dest] is localhost, default [tag] is [`any]. *)

val play : ?dest:Lwt_unix.sockaddr -> uri -> unit or_error

val queue : ?dest:Lwt_unix.sockaddr -> uri -> unit or_error

val get_album : ?dest:Lwt_unix.sockaddr -> uri -> string -> Track.t list or_error

val get_artist : ?dest:Lwt_unix.sockaddr -> uri -> string -> Album.t list or_error
