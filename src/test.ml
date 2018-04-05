
(* Main program *)

open Format
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
    if not ((Filename.check_suffix s ".m") || (Filename.check_suffix s ".nb")) then
      raise (Arg.Bad "no .m or .nb extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let lexbuf_from_channel ?(file="<stdin>") c =
  let lb = Sedlexing.Utf8.from_channel c in
  let curr_p = {
    Lexing.dummy_pos with Lexing.pos_fname = file; pos_cnum = 0;
  } in
  let () = Sedlexing.set_curr_p lb curr_p in
  lb

let manhir_run parser' lexer' lexbuf =
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.file in
  let lexer = Sedlexing.convert_for_menhir Lexer.token in
  (* let lexer = (fun lexbuf () ->
    let x = lexer lexbuf () in
      Format.eprintf "parse: %a \"%s\"@." Lexer.report_position lexbuf (Lexer.lexeme lexbuf);
      x) in *)
  (* let x = *)
  parser (lexer lexbuf)
  (* in eprintf "finished %a@." Lexer.report_position lexbuf; x *)

let () =
  let c = open_in file in
  let lb = lexbuf_from_channel ~file c in
  let f =
  try
    let f = manhir_run Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    f
  with
    | Lexer.Error s ->
        eprintf "lexical error: %a %s@." Lexer.report_position lb s;
        exit 1
    | e ->
        eprintf "Unexpected: %a %s@."  Lexer.report_position lb (Printexc.to_string e);
        exit 2
  in try Interp.file f
  with
    | Interp.Error s ->
        eprintf "Interpret error: %s@." s;
        exit 1
    | e ->
        eprintf "Unexpected: %s@." (Printexc.to_string e);
        exit 2
