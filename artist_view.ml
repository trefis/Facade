open Core.Std
open Lwt

open Types

let draw ctx state =
  let align = LTerm_geom.H_align_right in
  let bold_style = LTerm_style.({ none with bold = Some true }) in
  let style ?default row =
    if row <> state.View.curr_line then default else
    Some LTerm_style.({ bold_style with foreground = Some lblue })
  in
  let print row ?(align = LTerm_geom.H_align_left) str =
    LTerm_draw.draw_string_aligned ctx (3 + row) align ?style:(style row) str
  in
  LTerm_draw.draw_string_aligned ctx 1 LTerm_geom.H_align_center
    ?style:(style ~default:bold_style 0) state.View.name ;
  List.iteri state.View.albums ~f:(fun line item ->
    let line = line + 1 in
    let infos =
      let l = List.map item.Album.artists ~f:(fun a -> a.Artist.name) in
      let str = String.concat ~sep:", " l in
      if String.length str <= 50 then str else
        sprintf "%s..." (String.prefix str 47)
    in
    print line (sprintf "    %s" item.Album.name) ;
    print line ~align (sprintf "%s    " infos) ;
  )


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

let action ~key uri name =
  let result, msg =
    match key with
    | `Enter -> Network.play uri, sprintf "Playing album '%s'" name
    | (* `Space *) _ ->
      Network.queue uri, sprintf "Added album '%s' to playlist" name
  in
  lwt msg =
    result
    >|= function
    | Ok () -> msg
    | Error msg -> msg
  in
  raise_lwt (Transition (Error msg))

let handle env ~key ({ View. curr_line ; albums } as state) =
  let key = to_handled_keys (LTerm_key.code key) in
  let incr_line () =
    let line = min (List.length albums) (curr_line + 1) in
    state.View.curr_line <- line
  in
  match key with
  | `Up ->
    let line = max 0 (curr_line - 1) in
    state.View.curr_line <- line ;
    return ()
  | `Down -> return (incr_line ())
  | `Enter | `Space ->
    if curr_line = 0 then return () else
    let f i album =
      if i <> curr_line then return (i + 1) else
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
    lwt _ = Lwt_list.fold_left_s f 1 albums in
    return ()
  | _ ->
    return ()
