(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* Original source: [Cohttp_lwt_unix_net]
   Available at : https://github.com/avsm/ocaml-cohttp/blob/master/lwt/cohttp_lwt_unix_net.ml *)

open Lwt

(* Vanilla TCP connection *)
let connect sa =
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM
      0 in
  lwt () = Lwt_unix.connect fd sa in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  return (ic, oc)

let close (ic,oc) =
  let _ = try_lwt Lwt_io.close oc with _ -> return () in
  try_lwt Lwt_io.close ic with _ -> return ()

let with_connection sa k =
  connect sa
  >>= fun conn ->
  k conn
  >>= fun result ->
  close conn
  >>= fun () ->
  return result
