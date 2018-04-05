open Ast
open Parser
open Lexing
open Sedlexing

exception Error of string

let keywords_tbl = begin
  let h = Hashtbl.create 32 in
  List.iter (fun (s, tok) -> Hashtbl.add h s tok)
    ["True", CST (Cbool true);
     "False", CST (Cbool false);
     "Null", CST Cnone;];
  h
end

let id_or_kwd s = match Hashtbl.find_opt keywords_tbl s
    with | Some s -> s | None -> IDENT s

let report_position oc lb =
  let col p = p.pos_cnum - p.pos_bol in
  let (s, e) = Sedlexing.lexing_positions lb in
  let fname = if e.pos_fname = "" then "<untitled>" else e.pos_fname in
  Format.fprintf oc "at %s:" fname;
  if s.pos_lnum = e.pos_lnum then
    Format.fprintf oc "line %d:%d-%d" s.pos_lnum (col s) (col e)
  else
    Format.fprintf oc "line %d:%d-%d:%d" s.pos_lnum (col s) e.pos_lnum (col e)

let string_buffer = Buffer.create 1024

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let digit = [%sedlex.regexp? '0'..'9']
let ident = [%sedlex.regexp? letter, Star (letter | digit | '_')]
let integer = [%sedlex.regexp? Plus digit]
let comment = [%sedlex.regexp? "#", Star (Compl ('\n'))]
let symbols = [%sedlex.regexp? '+' | '-' | '*' | '/' | '%'
  | "==" | "!=" | "<" | "<=" | ">" | ">="
  | '=' | ":=" | "=." | "++" | "--"]

let lexeme = Sedlexing.Utf8.lexeme

let rec token lexbuf =
  match%sedlex lexbuf with
  | '\n'        -> token lexbuf
  | Plus (white_space | comment) -> token lexbuf
  | "(*"        -> comment_block lexbuf
  | symbols     -> OPSYM (lexeme lexbuf)
  | ident       -> id_or_kwd (lexeme lexbuf)
  | integer     -> let s = lexeme lexbuf in
      (try CST (Cint (int_of_string s))
      with _ -> raise (Error ("constant too large: " ^ s)))
  | "{|"        -> BLOCK_BEGIN
  | "|}"        -> BLOCK_END
  | '('         -> LP
  | ')'         -> RP
  | '['         -> LSQ
  | ']'         -> RSQ
  | '{'         -> LB
  | '}'         -> RB
  | ','         -> COMMA
  | ';'         -> SEMICOLON
  | ";;"        -> DOUBLESEMICOLON
  | '"'         -> CST (Cstring (parse_string lexbuf))
  | eof         -> EOF
  | _           -> assert false
and parse_string lexbuf =
  match%sedlex lexbuf with
  | '"'    -> let s = Buffer.contents string_buffer in
              Buffer.reset string_buffer; s
  | "\\n"  -> Buffer.add_char string_buffer '\n'; parse_string lexbuf
  | "\\\"" ->  Buffer.add_char string_buffer '"'; parse_string lexbuf
  | eof    -> raise (Error "unterminated string")
  | _      -> let s = lexeme lexbuf in Buffer.add_string string_buffer s; parse_string lexbuf
and comment_block lexbuf =
  match%sedlex lexbuf with
  | "*)"        -> token lexbuf
  | eof         -> raise (Error "unterminated comment")
  | _           -> comment_block lexbuf
