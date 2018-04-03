{
open Lexing
open Parser

exception Lexing_error of string

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
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = ' ' | '\t'
let comment = "#" [^'\n']*
let symbols = ('+' | '-' | '*' | '/' | '%'
  | "==" | "!=" | "<" | "<=" | ">" | ">="
  | '=' | ":=" | "=." | "++" | "--")

rule next_tokens = parse
  | '\n'    { new_line lexbuf; next_tokens lexbuf }
  | (space | comment)+
            { next_tokens lexbuf }
  | "(*"    { comment_block lexbuf }
  | ident as id { [id_or_kwd id] }
  | symbols as s
            { [OPSYM s] }
  | '('     { [LP] }
  | ')'     { [RP] }
  | '['     { [LSQ] }
  | ']'     { [RSQ] }
  | '{'     { [LB] }
  | '}'     { [RB] }
  | "[["    { [LDSQ] }
  | "]]"    { [RDSQ] }
  | "{{"    { [LDB] }
  | "}}"    { [RDB] }
  | ','     { [COMMA] }
  | ';'     { [SEMICOLON] }
  | ";;"    { [DOUBLESEMICOLON] }
  | integer as s
            { try [CST (Cint (int_of_string s))]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '"'     { [CST (Cstring (string lexbuf))] }
  | eof     { [EOF] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }
and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
        Buffer.reset string_buffer;
        s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
        string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
        string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
        string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }
and comment_block = parse
  | "*)"    { next_tokens lexbuf }
  | "\n"    { new_line lexbuf; comment_block lexbuf }
  | _       { comment_block lexbuf }


{
let next_token =
  let tokens = Queue.create () in (* next lexemes to send back *)
  fun lb ->
    if Queue.is_empty tokens then begin
      let l = next_tokens lb in
      List.iter (fun t -> Queue.add t tokens) l
    end;
    Queue.pop tokens
}
