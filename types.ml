open Core.Std

module J = Yojson.Basic

let convert_each' f = function
  | `Null -> []
  | json -> J.Util.convert_each f json

let to_string' = function
  | `Null -> ""
  | s -> J.Util.to_string s

module Artist = struct
  type t = { uri : string ; name : string }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string') in
    let name = J.Util.(member "name" j |> to_string) in
    { uri ; name }
end

module Album = struct
  type t  = { uri : string ; name : string ; artists : Artist.t list }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string') in
    let name = J.Util.(member "name" j |> to_string) in
    let artists = J.Util.(member "artists" j |> convert_each' Artist.from_json) in
    { uri ; name ; artists }
end

module Track = struct
  type t = {
    uri     : string ;
    name    : string ;
    artists : Artist.t list ;
    albums  : Album.t list ;
  }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string') in
    let name = J.Util.(member "name" j |> to_string) in
    let artists = J.Util.(member "artists" j |> convert_each' Artist.from_json) in
    let albums = J.Util.(member "albums" j |> convert_each' Album.from_json) in
    { uri ; name ; artists ; albums }
end

module SearchResult = struct
  type t = {
    source  : string ;
    artists : Artist.t list * bool ;
    albums  : Album.t list * bool ;
    tracks  : Track.t list * bool ;
  }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string) in
    let source =
      match String.split uri ~on:':' with
      | src :: _ -> src
      | _ -> assert false
    in
    let artists =
      J.Util.(member "artists" j |> convert_each' Artist.from_json), false
    in
    let albums =
      J.Util.(member "albums" j |> convert_each' Album.from_json), false
    in
    let tracks =
      J.Util.(member "tracks" j |> convert_each' Track.from_json), false
    in
    { source ; artists ; albums ; tracks }
end
