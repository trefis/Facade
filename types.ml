open Core.Std

module J = Yojson.Basic

let convert_each' f = function
  | `Null -> []
  | json -> J.Util.convert_each f json

let to_string' = function
  | `Null -> ""
  | s -> J.Util.to_string s

module Foldable = struct
  type 'a t = {
    values : 'a list ;
    mutable folded : bool ;
  }

  let toogle x = x.folded <- not x.folded
end

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
    artists : Artist.t Foldable.t ;
    albums  : Album.t Foldable.t ;
    tracks  : Track.t Foldable.t ;
  }

  let from_json j =
    let uri = J.Util.(member "uri" j |> to_string) in
    let source =
      match String.split uri ~on:':' with
      | src :: _ -> src
      | _ -> assert false
    in
    let mk values = { Foldable. values ; folded = true } in
    let artists = mk J.Util.(member "artists" j |> convert_each' Artist.from_json) in
    let albums = mk J.Util.(member "albums" j |> convert_each' Album.from_json) in
    let tracks = mk J.Util.(member "tracks" j |> convert_each' Track.from_json) in
    { source ; artists ; albums ; tracks  }
end

module View = struct
  type sr_state = {
    request : string ;
    results : SearchResult.t list ;
    mutable cursor_line    : int ;
    mutable screen_portion : int * int ;
  }

  type album_view_state = {
    name  : string ;
    uri   : string ;
    songs : Track.t list ;
    mutable curr_line : int ;
  }

  type artist_view_state = {
    name  : string ;
    uri   : string ;
    albums: Album.t list ;
    mutable curr_line : int ;
  }

  type t =
    | Main of CamomileLibrary.UChar.t Zipper.t
    | SR of sr_state
    | Album of album_view_state
    | Artist of artist_view_state
end

exception Transition of (View.t, string) Result.t

module Env = struct
  type t = View.t Zipper.t ref

  let init = Zipper.singleton (View.Main Zipper.empty)
end

