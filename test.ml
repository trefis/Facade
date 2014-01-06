open Core.Std
open Lwt

let () =
  Lwt_main.run (
    Mopidy.search ~tag:`artist "eminem"
    >>= function
    | Error e ->
      printf "Error: %s\n%!" e ;
      exit (-1)
    | Ok lst ->
      printf "Sources:" ;
      List.iter lst ~f:(fun sr ->
        let open Types in
        printf "\n  + %s" sr.SearchResult.source
      ) ;
      printf "\n%!" ;
      return ()
  )
