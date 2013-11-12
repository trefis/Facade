open Core.Std
open Lwt

module J = Yojson.Basic

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

let search ?(dest=localhost) ?(tag=`any) pattern =
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
      return (Ok (J.Util.convert_each Types.SearchResult.from_json answer))
    | "error" ->
      let err = J.Util.(index 1 json |> to_string) in
      return (Error err)
    | msg ->
      return (Error (sprintf "unknown answer kind '%s'" msg))
  )

let play ?(dest=localhost) uri =
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
