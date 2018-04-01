open Ast
open Parser

let make_ch f = fun _ s -> f s

let dump_obj oc = Printf.fprintf oc "{ \"type\": \"%s\", \"value\": %a }"
let rec dump_list oc sep d = function
  | x::[] -> Printf.fprintf oc "%a" d x
  | x::((_::_) as xs) -> Printf.fprintf oc "%a%s" d x sep; dump_list oc sep d xs
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
  | Sprint e -> dump_obj oc "stmt_print" dump_expr e; Printf.fprintf oc "\n"
  | Seval e -> dump_obj oc "stmt_eval" dump_expr e; Printf.fprintf oc "\n"
  | Sclear id -> dump_obj oc "stmt_clear" dump_ident id; Printf.fprintf oc "\n"
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
  | Cnone -> Printf.fprintf oc "null"
  | Cbool true -> Printf.fprintf oc "true"
  | Cbool false -> Printf.fprintf oc "false"
  | Cstring s -> Printf.fprintf oc "\"%s\"" (String.escaped s)
  | Cint i -> Printf.fprintf oc "%d" i
and dump_ident oc id = Printf.fprintf oc "\"%s\"" (String.escaped id)
and dump_expr_list oc l = Printf.fprintf oc "[%a]" (fun oc -> dump_list oc ", " dump_expr) l
and dump_call oc ty n el =
  let n = String.escaped (func_id_to_string n) in
  Printf.fprintf oc "{ \"type\": \"%s\", \"name\": \"%s\", \"args\": %a }" ty n dump_expr_list el

let dump_ast oc f =
  List.iter (dump_statement oc) f
