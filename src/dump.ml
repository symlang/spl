open Ast
open Parser

let make_ch f = fun _ s -> f s

module Debug = struct
let dump_obj oc = Format.fprintf oc "{ \"type\": \"%s\", \"value\": %a }"

let rec dump_statement oc = function
  | Sprint e -> dump_obj oc "stmt_print" dump_expr e;
  | Seval e -> dump_obj oc "stmt_eval" dump_expr e;
  | Sclear id -> dump_obj oc "stmt_clear" dump_ident id;
and dump_expr oc = function
  | Ecst c -> dump_obj oc "expr_const" dump_const c
  | Esymbol s -> dump_obj oc "expr_symbol" dump_symbol s
  | Ecall (e, el) -> dump_call oc "expr_call" e el
  | Elist el -> dump_obj oc "expr_list" dump_expr_list el
  | Eget (e1, e2) -> dump_obj oc "expr_get" dump_expr_list [e1; e2]
  | Eblock l -> dump_obj oc "expr_block" dump_expr_list l
and dump_const oc = function
  | Cnone -> Format.fprintf oc "null"
  | Cbool true -> Format.fprintf oc "true"
  | Cbool false -> Format.fprintf oc "false"
  | Cstring s -> Format.fprintf oc "\"%s\"" (String.escaped s)
  | Cint i -> Format.fprintf oc "%d" i
and dump_symbol oc = function
  | Cident id -> Format.fprintf oc "%a" dump_ident id
and dump_ident oc id = Format.fprintf oc "\"%s\"" (String.escaped id)
and dump_expr_list oc l = Format.fprintf oc "[%a]" (Utils.dump_alist ~sep:", " ~dump_func:dump_expr) l
and dump_call oc ty n el =
  Format.fprintf oc "{ \"type\": \"%s\", \"name\": %a, \"args\": %a }" ty dump_expr n dump_expr_list el

let dump_ast oc f =
  Format.fprintf oc "[\n%a\n]" (Utils.dump_alist ~sep:",\n" ~dump_func:dump_statement) f
end

module FullForm = struct
let dump_obj oc = Format.fprintf oc

let rec dump_statement oc = function
  | Sprint e -> dump_obj oc "%a;" dump_expr e;
  | Seval e -> dump_obj oc "%a;;" dump_expr e;
  | Sclear id -> dump_obj oc "Unset[%a]" dump_ident id;
and dump_expr oc = function
  | Ecst c -> dump_obj oc "%a" dump_const c
  | Esymbol s -> dump_obj oc "%a" dump_symbol s
  | Ecall (e, el) -> dump_call oc e el
  | Elist el -> dump_obj oc "List[%a]" dump_expr_list el
  | Eget (e1, e2) -> dump_obj oc "Get[%a]" dump_expr_list [e1; e2]
  | Eblock l -> dump_obj oc "{|%a|}" (Utils.dump_alist ~sep:"; " ~dump_func:dump_expr) l
and dump_const oc = function
  | Cnone -> Format.fprintf oc "Null"
  | Cbool true -> Format.fprintf oc "True"
  | Cbool false -> Format.fprintf oc "False"
  | Cstring s -> Format.fprintf oc "\"%s\"" (String.escaped s)
  | Cint i -> Format.fprintf oc "%d" i
and dump_symbol oc = function
  | Cident id -> Format.fprintf oc "%a" dump_ident id
and dump_ident oc id = Format.fprintf oc "%s" id
and dump_expr_list oc l = Format.fprintf oc "%a" (Utils.dump_alist ~sep:", " ~dump_func:dump_expr) l
and dump_call oc n el =
  Format.fprintf oc "%a[%a]" dump_expr n dump_expr_list el

let dump_ast oc f =
  Format.fprintf oc "{\n%a\n};" (Utils.dump_alist ~sep:",\n  " ~dump_func:dump_statement) f
end