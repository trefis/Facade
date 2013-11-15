open Core.Std
open Lwt
open Types

let draw ctx state =
  let line = state.View.cursor_line in
  if state.View.screen_portion = (0,0) then (
    let { LTerm_geom . rows ; cols } = LTerm_draw.size ctx in
    state.View.screen_portion <- (0, rows - 4)
  ) ;
  let min_r, max_r = state.View.screen_portion in
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
    (sprintf "Results for '%s':" state.View.request) ;
  let item = function true -> '+' | false -> '-' in
  let open Types in
  let foobar fmt ~line ~f x =
    let open Foldable in
    print line (fmt (item x.folded) (List.length x.values)) ;
    if x.folded then line + 1 else List.fold x.values ~init:(line + 1) ~f
  in
  ignore begin
    List.fold state.View.results ~init:0 ~f:(fun line result  ->
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

let to_handled_keys =
  let open LTerm_key in
  function
  | Up -> `Up
  | Down -> `Down
  | Enter -> `Enter
  | Char uchar ->
    let c = CamomileLibrary.UChar.char_of uchar in
    if c = ' ' then `Space else
    if c = 'j' then `Down else
    if c = 'k' then `Up else
      `NotHandled
  | _ -> `NotHandled

let nb_lines =
  List.fold ~init:0 ~f:(fun nb result ->
    let open SearchResult in
    let get_nb x = if x.Foldable.folded then 0 else List.length x.Foldable.values in
    let arts = get_nb result.artists in
    let albs = get_nb result.albums in
    let tras = get_nb result.tracks in
    nb + 4 + arts + albs + tras
  )

let handle env ~key ({ View. cursor_line = line ; _ } as state) =
  let key = to_handled_keys (LTerm_key.code key) in
  let (min_r, max_r) = state.View.screen_portion in
  match key with
  | `Up ->
    let line = max 0 (line - 1) in
    state.View.cursor_line <- line ;
    if min_r > line then
      state.View.screen_portion <- (min_r - 1, max_r - 1) ;
    return ()
  | `Down ->
    let line = min (nb_lines state.View.results) (line + 1) in
    state.View.cursor_line <- line ;
    if max_r < line then
      state.View.screen_portion <- (min_r + 1, max_r + 1) ;
    return ()
  | `Enter | `Space ->
    let toggled = ref false in
    let open Types in
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
        lwt l = foobar l result.artists ~f:(fun l _ -> return (l + 1)) in
        lwt l =
          let album_fun l album =
            if l <> line then return (l + 1) else
            let name = album.Album.name in
            let uri = album.Album.uri in
            lwt trans =
              Network.get_album uri name
              >|= function
              | Error msg -> Error msg
              | Ok songs ->
                Ok View.(Album { name ; uri ; songs ; curr_line = 0 })
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
                Network.play track.Track.uri,
                sprintf "Playing '%s'" track.Track.name
              | (* `Space *) _ ->
                Network.queue track.Track.uri,
                sprintf "Added '%s' to playlist" track.Track.name
            in
            state.View.cursor_line <- line + 1 ;
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
      Lwt_list.fold_left_s fold_result 0 state.View.results
    in
    return ()
  | _ -> return ()
