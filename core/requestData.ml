open ProcessTypes

type time = float
  [@@deriving show]

type flat_query_record = {
  query_string: string;
  query_time: time;
}
  [@@deriving show]


type nested_query_record = {
  subqueries: flat_query_record list;
  overall_time: time;
}
  [@@deriving show]

let sum_query_time =
  List.fold_left (fun acc x -> acc +. x.query_time) 0.0

type query_record =
  Flat of flat_query_record | Nested of nested_query_record
  [@@deriving show]

type request_data = {
  request_uri : string;
  cgi_parameters : (string * string) list ref;
  cookies : (string * string) list ref;
  http_response_headers : (string * string) list ref;
  http_response_code : int ref;
  queries : (query_record list) ref;
  client_id : client_id ref;
}
  [@@deriving show]

let new_empty_request_data () = {
  request_uri = "";
  cgi_parameters = ref [];
  cookies = ref [];
  http_response_headers = ref [];
  queries = ref [];
  http_response_code = ref 200;
  client_id = ref (dummy_client_id);
}

let new_request_data request_uri cgi_params cookies client_id = {
    request_uri;
    cgi_parameters = ref cgi_params;
    cookies = ref cookies;
    queries = ref [];
    http_response_headers = ref [];
    http_response_code = ref 200;
    client_id = ref client_id;
}


let get_request_uri rq = rq.request_uri

let get_cgi_parameters req_data = !(req_data.cgi_parameters)
let set_cgi_parameters req_data x = req_data.cgi_parameters := x

let get_cookies req_data = !(req_data.cookies)
let set_cookies req_data x = req_data.cookies := x

let get_http_response_headers req_data = !(req_data.http_response_headers)
let set_http_response_headers req_data x = req_data.http_response_headers := x

let get_http_response_code req_data = !(req_data.http_response_code)
let set_http_response_code req_data x = req_data.http_response_code := x

let get_client_id req_data = !(req_data.client_id)
let set_client_id req_data x = req_data.client_id := x

let query_record query_string query_time = { query_string; query_time }

(*
let string_of_flat_query_record qr =
  qr.query_string ^ " " ^ (Int64.to_string qr.query_time)
  *)

let add_flat_query_record rq query_string query_time =
  let fqr = Flat {query_string; query_time} in
  rq.queries := fqr :: !(rq.queries)

let add_nested_query_record rq fqrs overall_time =
  let subqueries = List.map (fun (q, time) -> query_record q time) fqrs in
  rq.queries := (Nested { subqueries; overall_time }) :: !(rq.queries)

let dump_query_metrics rq =
  let reduce (total_query_count, total_query_time, total_time) = function
    | Flat fqr ->
        let debug_str =
          Printf.sprintf "flat query: %s, time: %f\n" fqr.query_string fqr.query_time in
        Debug.print debug_str;
        (total_query_count + 1, total_query_time +. fqr.query_time, total_time +. fqr.query_time)
    | Nested nqr ->
        let query_count = List.length nqr.subqueries in
        let debug_str =
          Printf.sprintf "nested query. subquery count: %d, time: %f\n" query_count nqr.overall_time in
        Debug.print debug_str;
        (total_query_count + (List.length nqr.subqueries),
          total_query_time +. (sum_query_time nqr.subqueries),
          total_time +. nqr.overall_time) in

  let (count, execution_time, overall_time) = List.fold_left reduce (0, 0.0, 0.0) !(rq.queries) in
  Logging.(log query_time_logfile
    (Printf.sprintf "%s,%d,%f,%f" rq.request_uri count execution_time overall_time));
  let debug_str =
    Printf.sprintf "== Overall ==\nCount: %d\nTotal query time (ms): %f\n" count overall_time in
  Debug.print debug_str

module DecodeRequestHeaders =
  struct

let hexa_val conf =
  match conf with
    | '0'..'9' -> Char.code conf - Char.code '0'
    | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
    | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
    | _ -> 0

let raw_decode : string -> string = fun s ->
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
    | '%' | '+' -> true
    | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          | '%' when i + 2 < String.length s -> i + 3
          | _ -> succ i
      in
    compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in : bytes -> int -> int -> bytes = fun s1 i i1 ->
    if i < String.length s then
      let i =
        match s.[i] with
          | '%' when i + 2 < String.length s ->
              let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
        Bytes.set s1 i1 (Char.chr v); i + 3
          | '+' -> Bytes.set s1 i1  ' '; succ i
          | x -> Bytes.set s1 i1  x; succ i
      in
    copy_decode_in s1 i (succ i1)
    else s1
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    Bytes.to_string (copy_decode_in s1 0 0)
  else
    s

let decode : string -> string = fun s ->
  let rs = raw_decode s in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else
    s
    else
      s
  in
  strip_heading_and_trailing_spaces rs
end

(** remote client->server call *)
let is_remote_call params =
  List.mem_assoc "__name" params && List.mem_assoc "__args" params

(** return __result from server->client call with server continuation __continuation *)
let is_client_return params =
  List.mem_assoc "__continuation" params && List.mem_assoc "__result" params

let is_ajax_call cgi_args =
  (is_remote_call cgi_args) || (is_client_return cgi_args)
