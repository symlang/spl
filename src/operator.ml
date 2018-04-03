open Ast

exception Parsing_error of string

let prefix_call (op, e) = Ecall (Esymbol (Cprefix op), [e])
let binop_call (op, e1, e2) = Ecall (Esymbol (Cbinop op), [e1; e2])

type someop = [ `binop of binop | `preop of preop | `postop of postop ] 

let ops = [
  `preop Uneg, "-", "Minus", 480; (* BFlistable, BFnumericFunction *)
  `preop Uadd, "+", "Plus", 480; (* BFflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  (* "++", "PreIncrement", 660, BFholdFirst, BFreadProtected *)
  (* "--", "PreDecrement", 660, BFholdFirst, BFreadProtected *)
  (* "<<", "Get", 720, BFprotected *)
  (* "!", "Not", 230 *)

  `postop Uclear, "=.", "Unset", 670; (* BFholdFirst, BFlistable, FBreadProtected *)
  (* "&", "Function", 90, BFholdAll *)
  (* "++", Increment, 660, BFholdFirst, BFreadProtected *)
  (* "--", Decrement, 660, BFholdFirst, BFreadProtected *)
  (* "!", Factorial, 610 *)
  (* "..", Repeated, 170 *)
  (* "'", "Derivative", 670, BFnHoldAll *)

  `binop Badd, "+", "Plus", 310; (* BFflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  `binop Bsub, "-", "Subtract", 310; (* BFlistable, BFnumericFunction *)
  `binop Bmul, "*", "Times", 400; (* BFflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  `binop Bdiv, "/", "Divide", 470; (* BFlistable, BFnumericFunction *)
  (* "^", "Power", 590, BFlistable, BFnumericFunction, BFoneIdentity *)
  (* `binop Bdiv, "%", "Mod", 470; *)
  (* ".", "Dot", 490, BFflat, BFoneIdentity *)
  `binop Beq, "==", "Equal", 290; (* BFNonAssociative *)
  `binop Bneq, "!=", "NotEqual", 290; (* BFNonAssociative *)
  `binop Blt, "<", "Less", 290; (* BFNonAssociative *)
  `binop Ble, "<=", "LessEqual", 290; (* BFNonAssociative *)
  `binop Bgt, ">", "Greater", 290; (* BFNonAssociative *)
  `binop Bge, ">=", "GreaterEqual", 290; (* BFNonAssociative *)
  (* "===", "SameQ", 290 *)
  (* "=!=", "UnsameQ", 290 *)
  `binop Band, "&&", "And", 215; (* BFflat, BFholdAll, BFoneIdentity *)
  `binop Bor, "||", "Or", 215; (* BFflat, BFholdAll, BFoneIdentity *)
  (* u22BB, "Xor", 215, BFflat, BFholdAll, BFoneIdentity *)

  `binop Bassign, "=", "Set", 40; (* BFholdFirst, BFsequenceHold *)
  `binop Bdelay, ":=", "SetDelayed", 40;
  (* "^=", "UpSet", 40, BFholdFirst, BFsequenceHold *)
  (* "+=", "AddTo", 100, BFright, BFholdFirst *)
  (* "-=", "SubtractFrom", 100, BFright, BFholdFirst *)
  (* "*=", "TimesBy", 100, BFright, BFholdFirst *)
  (* "/=", "DevideBy", 100, BFright, BFholdFirst *)

  (* "->", "Rule", 120, BFright, BFsequenceHold *)
  (* ":>", "RuleDelayed", 120, BFright, BFsequenceHold, BFholdRest *)
  (* "/.", "ReplaceAll", 110, BFleft *)
  (* "//.", "ReplaceRepeated", 110, BFleft *)
  (* "?", "PatternTest", 680 *)
  (* "|", "Alternatives", 160 *)
  (* ":", "Optional", 140, BFright *)
  (* "/;", "Condition", 130, BFholdRest *)

  (* "/@", "Map", 620, BFright *)
  (* "@@", "Apply", 620, BFright *)
  (* "@@@", "ApplyLevel", 620, BFright *)
  (* uF523, "Implies", 200, BFright *)
  (* u29E6, "Equivalent", 205, BForderless *)
  (* "~~", "StringExpression", 135, BFflat, BFoneIdentity, BFprotected *)
  (* "<>", "StringJoin", 600, BFflat, BFoneIdentity *)
  (* ";;", "Span", 305 *)
  (* ";", "CompoundExpression", 10, BFholdAll, BFreadProtected *)
  (* ">>", "Put", 30 *)
  (* ">>>", "PutAppend", 30, BFprotected *)
  (* "//", "Postfix", 70, BFleft *)
  (* "@", "Prefix", 640, BFright *)
  (* "::", "MessageName", 750, BFholdFirst *)
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