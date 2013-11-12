open Core.Std
open Lwt
open CamomileLibrary
open Types

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

let subcontext ctx ~between =
  let { LTerm_geom. rows = r ; cols = c } = LTerm_draw.size ctx in
  let (l1, l2) = between in
  let start = if l1 >= 0 then l1 else r + l1 + 1 in
  let stop  = if l2 >= 0 then l2 else r + l2 + 1 in
  assert (start < stop) ;
  let rect = { LTerm_geom. row1 = start ; row2 = stop ; col1 = 0 ; col2 = c - 1 ; } in
  LTerm_draw.sub ctx rect

let draw_fun (env, err_opt) ui matrix =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx ;
  let main_view = subcontext ctx ~between:(0, -4) in
  let status_ln = subcontext ctx ~between:(-3, -1) in
  LTerm_draw.draw_hline status_ln 0 0 (LTerm_geom.cols size - 1) LTerm_draw.Light ;
  LTerm_draw.draw_string_aligned status_ln 1 LTerm_geom.H_align_left (hist_string !env) ;
  begin match Zipper.current !env with
  | Main str -> Main.draw main_view str
  | SearchResult (s, r, l) -> SRView.draw main_view s r l
  end ;
  match !err_opt with
  | None -> ()
  | Some (msg, t) ->
    if Time.(Span.(>=) (diff (now ()) t) (Span.of_sec 5.)) then err_opt := None else
    LTerm_draw.draw_string_aligned status_ln 1 LTerm_geom.H_align_right msg

let handle env err_opt ~key =
  let open LTerm_key in
  try_lwt
    begin match Zipper.current !env with
    | Main str -> Main.handle env ~key str
    | SearchResult (lst, req, line) -> SRView.handle env ~key lst req line
    | _ -> return ()
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
