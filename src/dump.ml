open Ast
open Parser

let make_ch f = fun _ s -> f s

let dump_obj ty d v = Printf.sprintf "{ \"type\": \"%s\", \"value\": %a }" ty (make_ch d) v

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

let rec dump_statement = function
  | Sprint e -> dump_obj "stmt_print" dump_expr e
  | Seval e -> dump_obj "stmt_eval" dump_expr e
and dump_expr = function
  | Ecst c -> dump_obj "expr_const" dump_const c
  | Eident id -> dump_obj "expr_ident" dump_ident id
  | Ebinop (op, e1, e2) -> dump_obj "expr_binop" dump_call (`binop (op), [e1; e2])
  | Eunop (op, e) -> dump_obj "expr_binop" dump_call (`unop (op), [e])
  | Ecall (id, el) -> dump_obj "expr_call" dump_call (`ident (id), el)
  | Elist el -> dump_obj "expr_list" dump_expr_list el
  | Eget (e1, e2) -> dump_obj "expr_get" dump_expr_list [e1; e2]
  | Eblock l -> dump_obj "expr_block" dump_expr_list l
and dump_const = function
  | Cnone -> "null"
  | Cbool true -> "true"
  | Cbool false -> "false"
  | Cstring s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Cint i -> Printf.sprintf "%d" i
and dump_ident id = Printf.sprintf "\"%s\"" (String.escaped id)
and dump_expr_list l = Printf.sprintf "[%s]" (String.concat ", " (List.map dump_expr l))
and dump_call (op, el) = Printf.sprintf "[\"%s\", %a]" (func_id_to_string op) (make_ch dump_expr_list) el

let dump_ast f =
  List.map dump_statement f
