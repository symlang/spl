open Ast
open Parser

let make_ch f = fun _ s -> f s

let dump_obj oc = Format.fprintf oc "{ \"type\": \"%s\", \"value\": %a }"
let rec dump_list oc ~sep ~dump_func = function
  | x::[] -> Format.fprintf oc "%a" dump_func x
  | x::((_::_) as xs) -> Format.fprintf oc "%a%s" dump_func x sep; dump_list oc ~sep ~dump_func xs
  | _ -> ()

type func_id = [`binop of binop|`unop of unop|`ident of string]

let unop_to_string = function
  | Uneg -> "Neg"
let binop_to_string = function
  | Badd -> "Add"
  | Bsub -> "Sub"
  | Bmul -> "Mul"
  | Bdiv -> "Div"
  | Bmod -> "Mod"
  | Beq -> "Equal"
  | Bneq -> "NotEqual"
  | Blt -> "LessThan"
  | Ble -> "LessEqual"
  | Bgt -> "GreatThan"
  | Bge -> "GreatEqual"
  | Band -> "And"
  | Bor -> "Or"
  | Bassign -> "Assign"
  | Bdelay -> "Delay"
let func_id_to_string = function
  | `binop op -> binop_to_string op
  | `unop op -> unop_to_string op
  | `ident i -> i

let rec dump_statement oc = function
  | Sprint e -> dump_obj oc "stmt_print" dump_expr e;
  | Seval e -> dump_obj oc "stmt_eval" dump_expr e;
  | Sclear id -> dump_obj oc "stmt_clear" dump_ident id;
and dump_expr oc = function
  | Ecst c -> dump_obj oc "expr_const" dump_const c
  | Eident id -> dump_obj oc "expr_ident" dump_ident id
  | Ebinop (op, e1, e2) -> dump_call oc "expr_binop" (`binop op) [e1; e2]
  | Eunop (op, e) -> dump_call oc "expr_unop" (`unop op) [e]
  | Ecall (id, el) -> dump_call oc "expr_call" (`ident id) el
  | Elist el -> dump_obj oc "expr_list" dump_expr_list el
  | Eget (e1, e2) -> dump_obj oc "expr_get" dump_expr_list [e1; e2]
  | Eblock l -> dump_obj oc "expr_block" dump_expr_list l
and dump_const oc = function
  | Cnone -> Format.fprintf oc "null"
  | Cbool true -> Format.fprintf oc "true"
  | Cbool false -> Format.fprintf oc "false"
  | Cstring s -> Format.fprintf oc "\"%s\"" (String.escaped s)
  | Cint i -> Format.fprintf oc "%d" i
and dump_ident oc id = Format.fprintf oc "\"%s\"" (String.escaped id)
and dump_expr_list oc l = Format.fprintf oc "[%a]" (dump_list ~sep:", " ~dump_func:dump_expr) l
and dump_call oc ty n el =
  let n = String.escaped (func_id_to_string n) in
  Format.fprintf oc "{ \"type\": \"%s\", \"name\": \"%s\", \"args\": %a }" ty n dump_expr_list el

let dump_ast oc f =
  Format.fprintf oc "[\n%a\n]" (dump_list ~sep:",\n" ~dump_func:dump_statement) f
