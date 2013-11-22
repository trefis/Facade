open Core.Std
open Lwt

let to_handled_keys =
  let open LTerm_key in
  function
  | Up -> `Up 1
  | Prev_page -> `Up 10
  | Down -> `Down 1
  | Next_page -> `Down 10
  | Enter -> `Enter
  | Char uchar ->
    let c = CamomileLibrary.UChar.char_of uchar in
    if c = ' ' then `Space else
    if c = 'j' then `Down 1 else
    if c = 'k' then `Up 1 else
      `NotHandled
  | _ -> `NotHandled

class ['a] t name (content : 'a list) get_infos main_action action = object
  method name = name
  val mutable curr_line = 0

  method draw ctx =
    let align = LTerm_geom.H_align_right in
    let bold_style = LTerm_style.({ none with bold = Some true }) in
    let style ?default row =
      if row <> curr_line then default else
        Some LTerm_style.({ bold_style with foreground = Some lblue })
    in
    let print row ?(align = LTerm_geom.H_align_left) str =
      LTerm_draw.draw_string_aligned ctx (3 + row) align ?style:(style row) str
    in
    LTerm_draw.draw_string_aligned ctx 1 LTerm_geom.H_align_center
      ?style:(style ~default:bold_style 0) name ;
    List.iteri content ~f:(fun line item ->
      let line = line + 1 in
      let name, infos = get_infos item in
      let infos =
        if String.length infos <= 50 then infos else
        sprintf "%s..." (String.prefix infos 47)
      in
      print line (sprintf "    %s" name) ;
      print line ~align (sprintf "%s    " infos) ;
    )

  method handle ~key =
    let key = to_handled_keys (LTerm_key.code key) in
    let incr_line ?(nb=1) () =
      let line = min (List.length content) (curr_line + nb) in
      curr_line <- line
    in
    match key with
    | `Up nb ->
      let line = max 0 (curr_line - nb) in
      curr_line <- line ;
      return ()
    | `Down nb -> return (incr_line ~nb ())
    | `Enter | `Space as k ->
      if curr_line = 0 then main_action k else
      let f i entry =
        if i <> curr_line then return (i + 1) else
        action k entry
      in
      lwt _ = Lwt_list.fold_left_s f 1 content in
      return ()
    | _ ->
      return ()
end
