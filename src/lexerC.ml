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
