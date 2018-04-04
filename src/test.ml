
(* Main program *)

open Format
open Sedlexing_menhir
open Parser

let usage = "usage: mini-python [options] file.m"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".m") then
      raise (Arg.Bad "no .m extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report e =
  eprintf "%s@." (string_of_ParseError e)

let () =
  let c = open_in file in
  let lb = Sedlexing.Utf8.from_channel c in
  let f =
  try
    let f = sedlex_with_menhir ~file Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    f
  with
    | Sedlexing_menhir.ParseError e ->
        report (current_position lb);
        eprintf "Parse error@.";
        exit 1
    | Lexer.Error s ->
        report (current_position lb);
        eprintf "lexical error: %s@." s;
        exit 1
    | e ->
        report (current_position lb);
        eprintf "Unexpected: %s\n@." (Printexc.to_string e);
        exit 2
  in try Interp.file f
  with
    | Interp.Error s ->
        eprintf "Interpret error: %s@." s;
        exit 1
    | e ->
        eprintf "Unexpected: %s\n@." (Printexc.to_string e);
        exit 2
