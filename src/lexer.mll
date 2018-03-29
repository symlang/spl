{
open Lexing
open Parser
open LexerC
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = ' ' | '\t'
let comment = "#" [^'\n']*

rule next_tokens = parse
  | '\n'    { new_line lexbuf; [NEWLINE] }
  | (space | comment)+
            { next_tokens lexbuf }
  | ident as id { [id_or_kwd id] }
  | '+'     { [PLUS] }
  | '-'     { [MINUS] }
  | '*'     { [TIMES] }
  | '/'     { [DIV] }
  | '%'     { [MOD] }
  | '='     { [ASSIGN] }
  | ":="    { [DELAY] }
  | "=="    { [CMP Beq] }
  | "!="    { [CMP Bneq] }
  | "<"     { [CMP Blt] }
  | "<="    { [CMP Ble] }
  | ">"     { [CMP Bgt] }
  | ">="    { [CMP Bge] }
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
