open Parser
open Sedlexing_menhir

exception Error of string

let keywords_tbl = begin
  let h = Hashtbl.create 32 in
  List.iter (fun (s, tok) -> Hashtbl.add h s tok)
    ["True", CST (Cbool true);
     "False", CST (Cbool false);
     "Null", CST Cnone;];
  h
end

let id_or_kwd s = try Hashtbl.find keywords_tbl s with Not_found -> IDENT s

let string_buffer = Buffer.create 1024

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let digit = [%sedlex.regexp? '0'..'9']
let ident = [%sedlex.regexp? letter, Star (letter | digit | '_')]
let integer = [%sedlex.regexp? Plus digit]
let space = [%sedlex.regexp? ' ' | '\t']
let comment = [%sedlex.regexp? "#", Star (Compl ('\n'))]
let symbols = [%sedlex.regexp? '+' | '-' | '*' | '/' | '%'
  | "==" | "!=" | "<" | "<=" | ">" | ">="
  | '=' | ":=" | "=." | "++" | "--"]

let rec token lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | white_space -> token lexbuf
  | _ -> assert false
