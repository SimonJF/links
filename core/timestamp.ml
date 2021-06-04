(* Timestamp datatype and timestamp parser entrypoints. *)
open Lexing
open Utility

type t = Timestamp of CalendarShow.t | MinusInfinity | Infinity
  [@@deriving ord]

let timestamp ts = Timestamp ts
let now () = Timestamp (CalendarShow.now ())
let infinity = Infinity
let minus_infinity = MinusInfinity

let pp ppf =
  let open Format in
  function
    | Timestamp ts  -> CalendarShow.pp ppf ts
    | Infinity      -> "'infinity'"
    | MinusInfinity -> "'-infinity'"

let parse_string str =
  let lexbuf = Lexing.from_string x in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "<string>" };
  try TimestampParser.timestamp TimestampLexer.read lexbuf with
    | SyntaxError msg -> raise (bad_date msg)
    | Parser.Error ->
        let open Format in
        fprintf str_formatter "%a" print_position lexbuf;
        let err = flush_str_formatter () in
        raise (bad_date err)
  
(** Parses a user timestamp string. Lack of an offset is assumed to mean
    local time. *)
let parse_user_string str =
  match parse_string str with
    | `Timestamp (cal, Some offset) ->
          CalendarShow.convert ts (Time_Zone.UTC_Plus n) (Time_Zone.UTC)
    | `Timestamp (cal, None) ->
          CalendarShow.convert ts (Time_Zone.Local) (Time_Zone.UTC)
    | `Infinity -> Infinity
    | `MinusInfinity -> MinusInfinity

(** Parses a database timestamp string. Lack of an offset is assumed to mean
    UTC. *)
let parse_db_string str =
  match parse_string str with
    | `Timestamp (cal, offset) ->
        let offset = OptionUtils.from_option 0 offset in
        CalendarShow.convert ts (Time_Zone.UTC_Plus offset) (Time_Zone.UTC)
    | `Infinity -> Infinity
    | `MinusInfinity -> MinusInfinity
