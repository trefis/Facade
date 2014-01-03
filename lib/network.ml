open Core.Std
open Lwt

type 'a or_error = ('a, string) Result.t t

module J = Yojson.Basic

let convert_each' f = function
  | `Null -> []
  | json -> J.Util.convert_each f json

let to_string' = function
  | `Null -> ""
  | s -> J.Util.to_string s

type uri = string

module Artist = struct
  type t = { uri : uri ; name : string }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string') in
    let name = J.Util.(member "name" j |> to_string) in
    { uri ; name }
end

module Album = struct
  type t  = { uri : uri ; name : string ; artists : Artist.t list }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string') in
    let name = J.Util.(member "name" j |> to_string) in
    let artists = J.Util.(member "artists" j |> convert_each' Artist.from_json) in
    { uri ; name ; artists }
end

module Track = struct
  type t = {
    uri     : uri ;
    name    : string ;
    artists : Artist.t list ;
    album  : Album.t ;
  }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string') in
    let name = J.Util.(member "name" j |> to_string) in
    let artists = J.Util.(member "artists" j |> convert_each' Artist.from_json) in
    let album = Album.from_json J.Util.(member "album" j) in
    { uri ; name ; artists ; album }
end

module SearchResult = struct
  type t = {
    source  : string ;
    artists : Artist.t list ;
    albums  : Album.t list ;
    tracks  : Track.t list ;
  }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string) in
    let source =
      match String.split uri ~on:':' with
      | src :: _ -> src
      | _ -> assert false
    in
    let artists = J.Util.(member "artists" j |> convert_each' Artist.from_json) in
    let albums = J.Util.(member "albums" j |> convert_each' Album.from_json) in
    let tracks = J.Util.(member "tracks" j |> convert_each' Track.from_json) in
    { source ; artists ; albums ; tracks  }
end

let localhost = Lwt_unix.ADDR_INET (Caml.Unix.inet_addr_loopback, 8881)

module Field = struct
  type t = [ `album | `artist | `track ]
  type with_extra = [ t | `any ]

  let to_string = function
    | `album -> "album"
    | `any -> "any"
    | `artist -> "artist"
    | `track -> "track"
end

let mk_query name json =
  let query = `List [ `String name ; json ] in
  J.to_string query

let search ?(dest=localhost) ?tag pattern =
  let tag =
    match tag with
    | None -> `any
    | Some t -> (t :> Field.with_extra)
  in
  try_lwt
    Tcp_client.with_connection dest (fun (ic, oc) ->
      let query =
	mk_query "search" (`Assoc [ (Field.to_string tag, `String pattern) ])
      in
      lwt () = Lwt_io.write_line oc query in
      lwt str = Lwt_io.read_line ic in
      let json = J.from_string str in
      match J.Util.(index 0 json |> to_string) with
      | "ok" ->
	let answer = J.Util.index 1 json in
	return (Ok (J.Util.convert_each SearchResult.from_json answer))
      | "error" ->
	let err = J.Util.(index 1 json |> to_string) in
	return (Error err)
      | msg ->
	return (Error (sprintf "unknown answer kind '%s'" msg))
    )
  with Unix.Unix_error _ ->
    return (Error "server not reachable")

let play ?(dest=localhost) uri =
  try_lwt
    Tcp_client.with_connection dest (fun (ic, oc) ->
      let query = mk_query "play" (`String uri) in
      lwt () = Lwt_io.write_line oc query in
      lwt str = Lwt_io.read_line ic in
      let json = J.from_string str in
      match J.Util.(index 0 json |> to_string) with
      | "ok" ->
	return (Ok ())
      | "error" ->
	let err = J.Util.(index 1 json |> to_string) in
	return (Error err)
      | msg ->
	return (Error (sprintf "unknown answer kind '%s'" msg))
    )
  with Unix.Unix_error _ ->
    return (Error "server not reachable")

let queue ?(dest=localhost) uri =
  try_lwt
    Tcp_client.with_connection dest (fun (ic, oc) ->
      let query = mk_query "queue" (`String uri) in
      lwt () = Lwt_io.write_line oc query in
      lwt str = Lwt_io.read_line ic in
      let json = J.from_string str in
      match J.Util.(index 0 json |> to_string) with
      | "ok" ->
	return (Ok ())
      | "error" ->
	let err = J.Util.(index 1 json |> to_string) in
	return (Error err)
      | msg ->
	return (Error (sprintf "unknown answer kind '%s'" msg))
    )
  with Unix.Unix_error _ ->
    return (Error "server not reachable")

let get_album ?(dest=localhost) uri name =
  try_lwt
    Tcp_client.with_connection dest (fun (ic, oc) ->
      let query =
        mk_query "get_album" (`Assoc [
          ("uri", `String uri) ;
          ("name", `String name) ;
        ])
      in
      lwt () = Lwt_io.write_line oc query in
      lwt str = Lwt_io.read_line ic in
      let json = J.from_string str in
      match J.Util.(index 0 json |> to_string) with
      | "ok" ->
	let answer = J.Util.index 1 json in
	return (Ok (J.Util.convert_each Track.from_json answer))
      | "error" ->
	let err = J.Util.(index 1 json |> to_string) in
	return (Error err)
      | msg ->
	return (Error (sprintf "unknown answer kind '%s'" msg))
    )
  with Unix.Unix_error _ ->
    return (Error "server not reachable")

let get_artist ?(dest=localhost) uri name =
  try_lwt
    Tcp_client.with_connection dest (fun (ic, oc) ->
      let query =
        mk_query "get_artist" (`Assoc [
          ("uri", `String uri) ;
          ("name", `String name) ;
        ])
      in
      lwt () = Lwt_io.write_line oc query in
      lwt str = Lwt_io.read_line ic in
      let json = J.from_string str in
      match J.Util.(index 0 json |> to_string) with
      | "ok" ->
	let answer = J.Util.index 1 json in
	return (Ok (J.Util.convert_each Album.from_json answer))
      | "error" ->
	let err = J.Util.(index 1 json |> to_string) in
	return (Error err)
      | msg ->
	return (Error (sprintf "unknown answer kind '%s'" msg))
    )
  with Unix.Unix_error _ ->
    return (Error "server not reachable")
