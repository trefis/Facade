open Core.Std
open Lwt

open Mopidy
open View

let artists_to_str lst =
  List.map lst ~f:(fun a -> a.Artist.name) |> String.concat ~sep:", "

let new_album_view uri name content =
  let get_infos { Track. name ; artists ; _ } = name, artists_to_str artists in
  let do_act key msg uri name =
    let result, msg =
      match key with
      | `Enter -> Mopidy.play uri, sprintf "Playing %s '%s'" msg name
      | `Space -> Mopidy.queue uri, sprintf "Added %s '%s' to playlist" msg name
    in
    lwt msg =
      result
      >|= function
      | Ok () -> msg
      | Error msg -> msg
    in
    raise_lwt (Transition (Error msg))
  in
  let main_action key = do_act key "album" uri name in
  let action key { Track. uri ; name ; _ } = do_act key "track" uri name in
  new Listing_view.t name content get_infos main_action action

let new_artist_view name content =
  let get_infos { Album. name ; artists ; _ } = name, artists_to_str artists in
  let action _key { Album. uri ; name ; _ } =
    let open Lwt in
    lwt view =
      Mopidy.get_album uri name
      >|= function
      | Error msg -> Error msg
      | Ok content -> Ok View.(Album (new_album_view uri name content))
    in
    raise_lwt (Transition view)
  in
  new Listing_view.t name content get_infos (fun _ -> return ()) action

let draw ctx state =
  let line = state.SearchResult.cursor_line in
  if state.SearchResult.screen_portion = (0,0) then (
    let { LTerm_geom . rows ; cols } = LTerm_draw.size ctx in
    state.SearchResult.screen_portion <- (0, rows - 4)
  ) ;
  let min_r, max_r = state.SearchResult.screen_portion in
  let print row ?(align = LTerm_geom.H_align_left) str =
    if row < min_r || row > max_r then () else
    let style =
      if row <> line then None else
      Some LTerm_style.({ none with foreground = Some lblue; bold = Some true })
    in
    LTerm_draw.draw_string_aligned ctx (3 + row - min_r) align ?style str
  in
  LTerm_draw.draw_string_aligned ctx 1 LTerm_geom.H_align_center
    ~style:LTerm_style.({ none with bold = Some true })
    (sprintf "Results for '%s':" state.SearchResult.request) ;
  let item = function true -> '+' | false -> '-' in
  let foobar fmt ~line ~f x =
    let open Foldable in
    print line (fmt (item x.folded) (List.length x.values)) ;
    if x.folded then line + 1 else List.fold x.values ~init:(line + 1) ~f
  in
  ignore begin
    List.fold state.SearchResult.results ~init:0 ~f:(fun line result  ->
      let open SearchResult in
      let align = LTerm_geom.H_align_right in
      print line (sprintf " Source : %s" result.source) ;
      let line = line + 1 in
      let line =
        foobar ~line (sprintf " %c %d artists") result.artists ~f:(fun l artist ->
          print l (sprintf "    %s" artist.Artist.name) ; l + 1
        )
      in
      let line =
        foobar ~line (sprintf " %c %d albums") result.albums ~f:(fun l album ->
          let authors =
            let l = List.map album.Album.artists ~f:(fun a -> a.Artist.name) in
            let str = String.concat ~sep:", " l in
            if String.length str <= 30 then str else
              sprintf "%s..." (String.prefix str 27)
          in
          print l (sprintf "    %s" album.Album.name) ;
          print l ~align (sprintf "%s  " authors) ;
          l + 1
        )
      in
      let line =
        foobar ~line (sprintf " %c %d tracks") result.tracks ~f:(fun l track ->
          let infos =
            let l = List.map track.Track.artists ~f:(fun a -> a.Artist.name) in
            let str =
              sprintf "(%s) %s" track.Track.album.Album.name
                (String.concat ~sep:", " l)
            in
            if String.length str <= 50 then str else
              sprintf "%s..." (String.prefix str 47)
          in
          print l (sprintf "    %s" track.Track.name) ;
          print l ~align (sprintf "%s    " infos) ;
          l + 1
        )
      in
      line + 1
    )
  end

let nb_lines =
  List.fold ~init:0 ~f:(fun nb result ->
    let open SearchResult in
    let get_nb x = if x.Foldable.folded then 0 else List.length x.Foldable.values in
    let arts = get_nb result.artists in
    let albs = get_nb result.albums in
    let tras = get_nb result.tracks in
    nb + 4 + arts + albs + tras
  )

let handle env ~key ({ SearchResult. cursor_line = line ; _ } as state) =
  let key = Listing_view.to_handled_keys (LTerm_key.code key) in
  let (min_r, max_r) = state.SearchResult.screen_portion in
  match key with
  | `Up nb ->
    let line = max 0 (line - nb) in
    state.SearchResult.cursor_line <- line ;
    if min_r > line then
      state.SearchResult.screen_portion <- (min_r - nb, max_r - nb) ;
    return ()
  | `Down nb ->
    let line = min (nb_lines state.SearchResult.results) (line + nb) in
    state.SearchResult.cursor_line <- line ;
    if max_r < line then
      state.SearchResult.screen_portion <- (min_r + nb, max_r + nb) ;
    return ()
  | `Enter | `Space ->
    let toggled = ref false in
    let foobar ~f l x =
      if !toggled then
        return l
      else if l = line then (
        toggled := true ;
        Foldable.toogle x ;
        return l
      ) else if x.Foldable.folded then
        return (l + 1)
      else
        Lwt_list.fold_left_s f (l + 1) x.Foldable.values
    in
    lwt _ =
      let fold_result l result =
        let open SearchResult in
        let l = l + 1 in
        lwt l = 
          let artist_fun l artist =
            if l <> line then return (l + 1) else
            let name = artist.Artist.name in
            let uri = artist.Artist.uri in
            lwt trans =
              Mopidy.get_artist uri name
              >|= function
              | Error msg -> Error msg
              | Ok content -> Ok View.(Artist (new_artist_view name content))
            in
            raise_lwt (Transition trans)
          in
          foobar l result.artists ~f:artist_fun
        in
        lwt l =
          let album_fun l album =
            if l <> line then return (l + 1) else
            let name = album.Album.name in
            let uri = album.Album.uri in
            lwt trans =
              Mopidy.get_album uri name
              >|= function
              | Error msg -> Error msg
              | Ok content -> Ok View.(Album (new_album_view uri name content))
            in
            raise_lwt (Transition trans)
          in
          foobar l result.albums ~f:album_fun
        in
        lwt l =
          let track_fun l track =
            if l <> line then return (l + 1) else
            let result, msg =
              match key with
              | `Enter ->
                Mopidy.play track.Track.uri,
                sprintf "Playing '%s'" track.Track.name
              | (* `Space *) _ ->
                Mopidy.queue track.Track.uri,
                sprintf "Added '%s' to playlist" track.Track.name
            in
            state.SearchResult.cursor_line <- line + 1 ;
            lwt msg =
              result
              >|= function
              | Ok () -> msg
              | Error msg -> msg
            in
            raise_lwt (Transition (Error msg))
          in
          foobar l result.tracks ~f:track_fun
        in
        return (l + 1)
      in
      Lwt_list.fold_left_s fold_result 0 state.SearchResult.results
    in
    return ()
  | _ -> return ()
