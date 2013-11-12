open Core.Std
open Lwt
open Types

let draw ctx lst request line =
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
  let item = function true -> '+' | false -> '-' in
  top := 5 ;
  let open Types in
  ignore begin
    List.fold lst ~init:0 ~f:(fun line result  ->
      incr top ;
      let open SearchResult in
      LTerm_draw.draw_string ctx (!top + line) 2 
        (sprintf "Source : %s" result.source) ;
      incr top ;
      let l = result.artists in
      print line (sprintf "%c %d artists" (item result.folded_art) (List.length l)) ;
      let line =
        if result.folded_art then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l artist ->
          print l (sprintf "    %s" artist.Artist.name) ; l + 1
        )
      in
      let l = result.albums in
      print line (sprintf "%c %d albums" (item result.folded_alb) (List.length l)) ;
      let line =
        if result.folded_alb then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l album ->
          print l (sprintf "    %s" album.Album.name) ; l + 1
        )
      in
      let l = result.tracks in
      print line (sprintf "%c %d tracks" (item result.folded_tra) (List.length l)) ;
      let line =
        if result.folded_tra then line + 1 else
        List.fold l ~init:(line + 1) ~f:(fun l track ->
          print l (sprintf "    %s" track.Track.name) ; l + 1
        )
      in
      line
    )
  end

let handle env ~key lst request line =
  let open LTerm_key in
  begin match LTerm_key.code key with
  | Up ->
    env := Zipper.set_current !env (SearchResult (lst, request, line - 1)) ;
    return ()
  | Down ->
    env := Zipper.set_current !env (SearchResult (lst, request, line + 1)) ;
    return ()
  | Enter ->
    let toggled = ref false in
    let open Types in
    lwt _ =
      let fold_result l result =
        let open SearchResult in
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
            lwt msg =
              Network.play track.Track.uri
              >>= function
              | Ok () -> return (sprintf "Playing '%s'" track.Track.name)
              | Error msg -> return msg
            in
            env := Zipper.set_current !env (SearchResult (lst, request, line + 1)) ;
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
        return l
      in
      Lwt_list.fold_left_s fold_result 0 lst
    in
    return ()
  | _ -> return ()
  end
