open Core.Std
open Lwt

open CamomileLibrary

type view =
  | Main of UChar.t Zipper.t
  | SearchResult of Types.SearchResult.t list * string * int

let get_string (z : UChar.t Zipper.t) =
  let l = List.map (Zipper.to_list z) ~f:UPervasives.escaped_uchar in
  String.concat l

module Env = struct
  type t = view Zipper.t

  let init = Zipper.singleton (Main Zipper.empty)
end

let hist_string =
  Zipper.fold ~init:"" ~f:(fun is_current acc elt ->
    let separator = if is_current then "»»" else "»" in
    match elt with
    | Main _ -> sprintf "%s %s search" acc separator
    | SearchResult (_, request, _) -> sprintf "%s %s %s" acc separator request
  )

let draw_search_string ctx ~at str =
  let highlight = ref false in
  let style = LTerm_style.({ none with background = Some lwhite }) in 
  let (line, init) = at in
  let last_col =
    Zipper.fold str ~init ~f:(fun is_current col elt ->
      let style = if !highlight then Some style else None in
      highlight := is_current ;
      LTerm_draw.draw_char ctx line col ?style elt ;
      col + 1
    )
  in
  if !highlight || last_col = init then
    LTerm_draw.draw_char ctx line last_col ~style (UChar.of_char ' ')

let subcontext ctx ~between =
  let { LTerm_geom. rows = r ; cols = c } = LTerm_draw.size ctx in
  let (l1, l2) = between in
  let start = if l1 >= 0 then l1 else r + l1 + 1 in
  let stop  = if l2 >= 0 then l2 else r + l2 + 1 in
  assert (start < stop) ;
  let rect = { LTerm_geom. row1 = start ; row2 = stop ; col1 = 0 ; col2 = c - 1 ; } in
  LTerm_draw.sub ctx rect

let draw_main ctx search_string =
  let { LTerm_geom. rows = r ; cols = c } = LTerm_draw.size ctx in
  let rect =
    let open LTerm_geom in {
      row1 = r / 2 - 2 ;
      row2 = r / 2 + 1 ;
      col1 = if c > 50 then 10 else 0 ;
      col2 = if c > 50 then c - 10 else c ;
    }
  in
  let str = get_string search_string in
  LTerm_draw.draw_string_aligned ctx (r / 2 - 4) LTerm_geom.H_align_center
    ~style:LTerm_style.({ none with bold = Some true }) "Search :";
  LTerm_draw.draw_frame ctx rect LTerm_draw.Light ;
  ignore (draw_search_string ctx ~at:(r / 2 - 1, if c > 50 then 12 else 2) search_string)

let draw_results ctx (lst, request, line) =
  let { LTerm_geom. rows = r ; cols = c } = LTerm_draw.size ctx in
  let top = ref 2 in (* no questions please. *)
  let print row str =
    let style =
      if row <> line then None else
      Some LTerm_style.({ none with foreground = Some lblue; bold = Some true })
    in
    LTerm_draw.draw_string ctx (!top + row) 5 ?style str
  in
  LTerm_draw.draw_string_aligned ctx !top LTerm_geom.H_align_center
    ~style:LTerm_style.({ none with bold = Some true })
    (sprintf "Results for '%s':" request) ;
  let item = function true -> '-' | false -> '+' in
  top := 5 ;
  let open Types in
  ignore begin
    List.fold lst ~init:0 ~f:(fun line result  ->
      incr top ;
      LTerm_draw.draw_string ctx (!top + line) 2 
        (sprintf "Source : %s" result.SearchResult.source) ;
      incr top ;
      let l, opened = result.SearchResult.artists in
      print line (sprintf "%c %d artists" (item opened) (List.length l)) ;
      let line =
        if not opened then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l artist ->
          print l (sprintf "    %s" artist.Artist.name) ; l + 1
        )
      in
      let l, opened = result.SearchResult.albums in
      print line (sprintf "%c %d albums" (item opened) (List.length l)) ;
      let line =
        if not opened then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l album ->
          print l (sprintf "    %s" album.Album.name) ; l + 1
        )
      in
      let l, opened = result.SearchResult.tracks in
      print line (sprintf "%c %d tracks" (item opened) (List.length l)) ;
      let line =
        if not opened then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l track ->
          print l (sprintf "    %s" track.Track.name) ; l + 1
        )
      in
      line
    )
  end

let draw_fun (env, err_opt) ui matrix =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx ;
  let main_view = subcontext ctx ~between:(0, -4) in
  let status_ln = subcontext ctx ~between:(-3, -1) in
  LTerm_draw.draw_hline status_ln 0 0 (LTerm_geom.cols size - 1) LTerm_draw.Light ;
  LTerm_draw.draw_string_aligned status_ln 1 LTerm_geom.H_align_left (hist_string !env) ;
  begin match Zipper.current !env with
  | Main str               -> draw_main main_view str
  | SearchResult (s, r, l) -> draw_results main_view (s, r, l)
  end ;
  match !err_opt with
  | None -> ()
  | Some (msg, t) ->
    if Time.(Span.(>=) (diff (now ()) t) (Span.of_sec 5.)) then err_opt := None else
    LTerm_draw.draw_string_aligned status_ln 1 LTerm_geom.H_align_right msg

exception Transition of (view, string) Result.t

let handle env err_opt ~key =
  let open LTerm_key in
  try_lwt
    begin match Zipper.current !env with
    | Main str ->
      lwt str' =
        match LTerm_key.code key with
        | Enter ->
          let str = get_string str in
          lwt res = Network.search str in
          let res = Result.map res ~f:(fun x -> SearchResult (x, str, 0)) in
          raise (Transition res)
        | Backspace  -> return (Zipper.delete str `before)
        | Delete     -> return (Zipper.delete str `after)
        | Left       -> return (Zipper.backward str)
        | Right      -> return (Zipper.forward str)
        | Char uchar -> return (Zipper.insert str uchar `after)
        | _ -> return str
      in
      if not (phys_equal str str') then
        env := Zipper.set_current !env (Main str') ;
      return_unit
    | SearchResult (lst, request, line) ->
      lwt lst, line =
        match LTerm_key.code key with
        | Up -> return (lst, line - 1)
        | Down -> return (lst, line + 1)
        | Enter ->
          let toggled = ref false in
          let open Types in
          lwt _, lst =
            let fold_result (l, acc) result =
              let l, artists =
                let lst, opened = result.SearchResult.artists in
                if !toggled then l, (lst, opened) else
                if l = line then (toggled := true ; l, (lst, not opened)) else
                if not opened then l + 1, (lst, opened) else
                List.fold lst ~init:(l + 1) ~f:(fun l _artist -> l + 1), (lst, opened)
              in
              let l, albums =
                let lst, opened = result.SearchResult.albums in
                if !toggled then l, (lst, opened) else
                if l = line then (toggled := true ; l, (lst, not opened)) else
                if not opened then l + 1, (lst, opened) else
                List.fold lst ~init:(l + 1) ~f:(fun l _album -> l + 1), (lst, opened)
              in
              lwt l, tracks =
                let lst, opened = result.SearchResult.tracks in
                if !toggled then return (l, (lst, opened)) else
                if l = line then (toggled := true ; return (l, (lst, not opened))) else
                if not opened then return (l + 1, (lst, opened)) else
                let track_fun l track =
                  if l <> line then return (l + 1) else
                  lwt msg =
                    Network.play track.Track.uri
                    >>= function
                    | Ok () -> return (sprintf "Playing '%s'" track.Track.name)
                    | Error msg -> return msg
                  in
                  raise (Transition (Error msg))
                in
                lwt l = Lwt_list.fold_left_s track_fun (l + 1) lst in
                return (l, (lst, opened))
              in
              return (l, { result with SearchResult. artists ; albums ; tracks } :: acc)
            in
            Lwt_list.fold_left_s fold_result (0, []) lst
          in
          return (List.rev lst, line)
        | _ -> return (lst, line)
      in
      env := Zipper.set_current !env (SearchResult (lst, request, line)) ;
      return_unit
    | _ -> return_unit
    end
  with
  | Transition (Ok view) ->
    env := Zipper.insert (Zipper.drop_tail !env) view `after ;
    return_unit
  | Transition (Error msg) ->
    err_opt := Some (msg, Time.now ()) ;
    return_unit

let rec loop ui env err_opt =
  let open LTerm_key in
  LTerm_ui.wait ui
  >>= function
  | LTerm_event.Key { code = Escape ; _ } -> return ()

  | LTerm_event.Key key ->
    lwt () =
      match key.code with
      | F2 -> env := Zipper.backward !env ; return_unit
      | F3 -> env := Zipper.forward !env ; return_unit
      | _ -> handle ~key env err_opt
    in
    LTerm_ui.draw ui ;
    loop ui env err_opt

  | _ -> loop ui env err_opt
  
let _lwt_unit =
  let initial_env = ref Env.init in
  let initial_err = ref None in
  lwt term = Lazy.force LTerm.stdout in
  lwt ui   = LTerm_ui.create term (draw_fun (initial_env, initial_err)) in
  try_lwt
    loop ui initial_env initial_err
  finally
    LTerm_ui.quit ui

let () = Lwt_main.run _lwt_unit
