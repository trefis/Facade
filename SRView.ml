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
  ignore begin
    List.fold state.View.results ~init:0 ~f:(fun line result  ->
      let open SearchResult in
      let open Foldable in
      let align = LTerm_geom.H_align_right in
      print line (sprintf " Source : %s" result.source) ;
      let line = line + 1 in
      let x = result.artists in
      print line (sprintf " %c %d artists" (item x.folded) (List.length x.values)) ;
      let line =
        if x.folded then line + 1 else
        List.fold x.values ~init:(line + 1) ~f:(fun l artist ->
          print l (sprintf "    %s" artist.Artist.name) ; l + 1
        )
      in
      let x = result.albums in
      print line (sprintf " %c %d albums" (item x.folded) (List.length x.values)) ;
      let line =
        if x.folded then line + 1 else
        List.fold x.values ~init:(line + 1) ~f:(fun l album ->
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
      let x = result.tracks in
      print line (sprintf " %c %d tracks" (item x.folded) (List.length x.values));
      let line =
        if x.folded then line + 1 else
        List.fold x.values ~init:(line + 1) ~f:(fun l track ->
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

let to_handled_keys = function
  | LTerm_key.Up -> `Up
  | LTerm_key.Down -> `Down
  | LTerm_key.Enter -> `Enter
  | LTerm_key.Char uchar when CamomileLibrary.UChar.char_of uchar = ' ' ->
    `Space
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
    lwt _ =
      let fold_result l result =
        let open SearchResult in
        let l = l + 1 in
        let l =
          if !toggled then
            l
          else if l = line then (
            toggled := true ;
            Foldable.toogle result.artists ;
            l
          ) else if result.artists.Foldable.folded then
            l + 1
          else
            List.fold result.artists.Foldable.values ~init:(l + 1) ~f:(fun l _artist -> l + 1)
        in
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
          if !toggled then
            return l
          else if l = line then (
            toggled := true ;
            Foldable.toogle result.albums ;
            return l
          ) else if result.albums.Foldable.folded then
            return (l + 1)
          else
            Lwt_list.fold_left_s album_fun (l + 1) result.albums.Foldable.values
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
          if !toggled then
            return l
          else
          if l = line then (
            toggled := true ;
            Foldable.toogle result.tracks ;
            return l
          ) else if result.tracks.Foldable.folded then
            return (l + 1)
          else
            Lwt_list.fold_left_s track_fun (l + 1) result.tracks.Foldable.values
        in
        return (l + 1)
      in
      Lwt_list.fold_left_s fold_result 0 state.View.results
    in
    return ()
  | _ -> return ()
