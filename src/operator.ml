open Ast

exception Parsing_error of string

let prefix_call (op, e) = Ecall (Esymbol (Cprefix op), [e])
let binop_call (op, e1, e2) = Ecall (Esymbol (Cbinop op), [e1; e2])

type someop = [ `binop of binop | `preop of preop | `postop of postop ] 

let ops = [
  `preop Uneg, "-", "Minus", 480;
  `preop Uadd, "+", "Plus", 480;
  (* "++", PreIncrement, 660 *)
  (* "--", PreDecrement, 660 *)

  `postop Uclear, "=.", "Unset", 670;
  (* "++", Increment, 660 *)
  (* "--", Decrement, 660 *)

  `binop Badd, "+", "Plus", 310;
  `binop Bsub, "-", "Subtract", 310;
  `binop Bmul, "*", "Times", 400;
  `binop Bdiv, "/", "Divide", 470;
  (* `binop Bdiv, "%", "Mod", 470; *)
  `binop Beq, "==", "Equal", 290;
  `binop Bneq, "!=", "NotEqual", 290;
  `binop Blt, "<", "Less", 290;
  `binop Ble, "<=", "LessEqual", 290;
  `binop Bgt, ">", "Greater", 290;
  `binop Bge, ">=", "GreaterEqual", 290;
  `binop Band, "&&", "And", 215;
  `binop Bor, "||", "Or", 215;
  `binop Bassign, "=", "Set", 40;
  `binop Bdelay, ":=", "SetDelayed", 40;
]


let preop_table = begin
  let t = Hashtbl.create 17 in
  List.iter (fun (op, s, n, p) ->
    match op with
    | `preop o -> Hashtbl.add t s (o, n, p)
    | _ -> ()) ops;
  t
end

let postop_table = begin
  let t = Hashtbl.create 17 in
  List.iter (fun (op, s, n, p) ->
    match op with
    | `postop o -> Hashtbl.add t s (o, n, p)
    | _ -> ()) ops;
  t
end

let binop_table = begin
  let t = Hashtbl.create 17 in
  List.iter (fun (op, s, n, p) ->
    match op with
    | `binop o -> Hashtbl.add t s (o, n, p)
    | _ -> ()) ops;
  t
end


let op_of_string t s = let (op, _, _) = Hashtbl.find t s in op
let preop_of_string = op_of_string preop_table
let postop_of_string = op_of_string postop_table
let binop_of_string = op_of_string binop_table


let parse_expr_virtual_list el =
  let rec pel = function
    | (EVexpr e)::[] -> e
    | (EVexpr e)::(EVsymbol op)::xs -> binop_call (binop_of_string op, e, pel xs)
    | _ -> raise (Parsing_error "extra operator") in
  pel el