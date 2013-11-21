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


