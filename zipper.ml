open Core.Std

type 'a t = {
  prev : 'a list ;
  next : 'a list ;
}

let empty = { prev = [] ; next = [] }
let singleton curr = { prev = [] ; next = [ curr ] }

let set_current { prev ; next } value =
  match next with
  | [] -> invalid_arg "Zipper.set_current"
  | _curr :: rest -> { prev ; next = value :: rest }

let current { prev ; next } =
  match next with
  | [] -> invalid_arg "Zipper.current"
  | curr :: _ -> curr

let insert { prev ; next } x pos =
  match pos with
  | `before -> { prev ; next = x :: next }
  | `after  ->
    match next with
    | [] -> { prev ; next = [ x ] }
    | curr :: rest -> { prev = curr :: prev ; next = x :: rest }

let delete ({ prev ; next } as t) pos =
  match pos with
  | `before ->
    begin match prev, next with
    | _, [] -> t
    | [], x :: next  -> { prev = [] ; next }
    | new_curr :: prev, _curr :: nexts ->
      { prev ; next = new_curr :: nexts }
    end
  | `after ->
    begin match next with
    | [] -> t
    | x :: xs -> { prev ; next = xs }
    end

let forward ({ prev ; next } as t) =
  match next with
  | [] | [ _ ]-> t
  | x :: xs -> { prev = x :: prev ; next = xs }

let backward { prev ; next } =
  match prev with
  | [] -> { prev ; next }
  | x :: xs -> { prev = xs ; next = x :: next }

let drop_tail ({ prev ; next } as t) =
  match next with
  | [] | [ _ ] -> t
  | x :: xs -> { prev ; next = [ x ] }

let to_list { prev ; next } = List.rev_append prev next

let fold { prev ; next } ~init ~f =
  match next with
  | [] -> init (* no current element -> zipper is empty *)
  | curr :: nexts ->
    let acc = List.fold (List.rev prev) ~init ~f:(f false) in
    let acc = f true acc curr in
    List.fold nexts ~init:acc ~f:(f false)

let iter t ~f = fold t ~init:() ~f:(fun b acc -> f b)
