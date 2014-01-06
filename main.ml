open Core.Std
open Lwt
open CamomileLibrary

open View

let get_string (z : UChar.t Zipper.t) =
  let l = List.map (Zipper.to_list z) ~f:UPervasives.escaped_uchar in
  String.concat l

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

let draw ctx search_string =
  let { LTerm_geom. rows = r ; cols = c } = LTerm_draw.size ctx in
  let rect =
    let open LTerm_geom in {
      row1 = r / 2 - 2 ;
      row2 = r / 2 + 1 ;
      col1 = if c > 50 then 10 else 0 ;
      col2 = if c > 50 then c - 10 else c ;
    }
  in
  LTerm_draw.draw_string_aligned ctx (r / 2 - 4) LTerm_geom.H_align_center
    ~style:LTerm_style.({ none with bold = Some true }) "Search :";
  LTerm_draw.draw_frame ctx rect LTerm_draw.Light ;
  ignore (draw_search_string ctx ~at:(r / 2 - 1, if c > 50 then 12 else 2) search_string)

let handle env ~key str =
  lwt str' =
    let open LTerm_key in
    match LTerm_key.code key with
    | Enter ->
      let request = get_string str in
      lwt res = Mopidy.search request in
      let f results = SR (SearchResult.make_state request results) in
      let res = Result.map res ~f in
      raise_lwt (Transition res)
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
