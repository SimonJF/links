open Lwt

let start_time =
  let open Unix in
  let tm = gettimeofday () |> localtime in
  Printf.sprintf "%d-%d-%d_%d-%d"
    (tm.tm_year + 1900) tm.tm_mon tm.tm_mday tm.tm_hour tm.tm_min

let get_time () = Unix.gettimeofday ()

let diff_time t1 t2 =
  (t2 -. t1) *. 1000.0

let time f =
  let t1 = get_time () in
  let res = f () in
  let t2 = get_time () in
  let diff = diff_time t1 t2 in
  (res, diff)

let time_lwt f =
  Lwt.return () >>= fun _ ->
  let t1 = get_time () in
  f () >>= fun res ->
  let t2 = get_time () in
  let diff = diff_time t1 t2 in
  Lwt.return (res, diff)

let page_time_logfile =
  Printf.sprintf "links_pagetime_%s.csv" start_time

let query_time_logfile =
  Printf.sprintf "links_querytime_%s.csv" start_time

let log filename content =
  let ch = open_out_gen [Open_append; Open_creat] 0o666 filename in
  Printf.fprintf ch "%s\n" content;
  close_out ch


