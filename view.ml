open Core.Std

module Foldable = struct
  type 'a t = {
    values : 'a list ;
    mutable folded : bool ;
  }

  let toogle x = x.folded <- not x.folded
end

module SearchResult = struct
  type t = {
    source  : string ;
    artists : Mopidy.Artist.t Foldable.t ;
    albums  : Mopidy.Album.t Foldable.t ;
    tracks  : Mopidy.Track.t Foldable.t ;
  }

  let fold values = { Foldable. values ; folded = true }

  let mk_foldable { Mopidy.SearchResult. source; artists; albums; tracks } = {
    source ;
    artists = fold artists ;
    albums = fold albums ;
    tracks = fold tracks ;
  }

  type state = {
    request : string ;
    results : t list ;
    mutable cursor_line    : int ;
    mutable screen_portion : int * int ;
  }

  let make_state request results = {
    request ;
    results = List.map results ~f:mk_foldable ;
    cursor_line = 0 ;
    screen_portion  = 0,0 ;
  }
end

type t =
  | Main of CamomileLibrary.UChar.t Zipper.t
  | SR of SearchResult.state
  | Album of Mopidy.Track.t Listing_view.t
  | Artist of Mopidy.Album.t Listing_view.t
type kind = t

exception Transition of (t, string) Result.t

module Env = struct
  type t = kind Zipper.t ref

  let init = Zipper.singleton (Main Zipper.empty)
end

