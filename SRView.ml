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
      let align = LTerm_geom.H_align_right in
      print line (sprintf " Source : %s" result.source) ;
      let line = line + 1 in
      let l = result.artists in
      print line (sprintf " %c %d artists" (item result.folded_art) (List.length l)) ;
      let line =
        if result.folded_art then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l artist ->
          print l (sprintf "    %s" artist.Artist.name) ; l + 1
        )
      in
      let l = result.albums in
      print line (sprintf " %c %d albums" (item result.folded_alb) (List.length l)) ;
      let line =
        if result.folded_alb then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l album ->
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
      let l = result.tracks in
      print line (sprintf " %c %d tracks" (item result.folded_tra) (List.length l)) ;
      let line =
        if result.folded_tra then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l track ->
          let authors =
            let l = List.map track.Track.artists ~f:(fun a -> a.Artist.name) in
            let str = String.concat ~sep:", " l in
            if String.length str <= 30 then str else
              sprintf "%s..." (String.prefix str 27)
          in
          print l (sprintf "    %s" track.Track.name) ;
          print l ~align (sprintf "%s  " authors) ;
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
    let arts = if result.folded_art then 0 else List.length result.artists in
    let albs = if result.folded_alb then 0 else List.length result.albums in
    let tras = if result.folded_tra then 0 else List.length result.tracks in
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
          let lst = result.artists in
          if !toggled then
            l
          else if l = line then (
            toggled := true ;
            result.folded_art <- not result.folded_art ;
            l
          ) else if result.folded_art then
            l + 1
          else
            List.fold lst ~init:(l + 1) ~f:(fun l _artist -> l + 1)
        in
        let l =
          let lst = result.albums in
          if !toggled then
            l
          else if l = line then (
            toggled := true ;
            result.folded_alb <- not result.folded_alb ;
            l
          ) else if result.folded_alb then
            l + 1
          else
            List.fold lst ~init:(l + 1) ~f:(fun l _album -> l + 1)
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
          let lst = result.tracks in
          if !toggled then
            return l
          else
          if l = line then (
            toggled := true ;
            result.folded_tra <- not result.folded_tra ;
            return l
          ) else if result.folded_tra then
            return (l + 1)
          else
            Lwt_list.fold_left_s track_fun (l + 1) lst
        in
        return (l + 1)
      in
      Lwt_list.fold_left_s fold_result 0 state.View.results
    in
    return ()
  | _ -> return ()
