open Ast

exception Parsing_error of string

type preop =
  | Unoop
  | Uneg | Uadd

type postop = | Uendop | Uclear

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)
  | Bassign | Bdelay                    (* = := *)

type someop = [ `binop of binop | `preop of preop | `postop of postop ]
type association = BFleft | BFright | BFnonassoc | BFflat | BFinequality | BFnone
type precedence = (int * association)

type vexpr =
  | EVexpr of expr
  | EVsymbol of string
  | EVop of (someop * ident * precedence)

(* TODO: association of the same precedence should be the same *)
let ops = [
  `preop Unoop, "\\[noop]", "", (0, BFnone);
  `preop Uneg, "-", "Minus", (480, BFright); (* BFlistable, BFnumericFunction *)
  `preop Uadd, "+", "Plus", (480, BFflat); (* BFflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  (* "++", "PreIncrement", 660, BFholdFirst, BFreadProtected *)
  (* "--", "PreDecrement", 660, BFholdFirst, BFreadProtected *)
  (* "<<", "Get", 720, BFnone, BFprotected *)
  (* "!", "Not", 230 *)

  `postop Uendop, "\\[endop]", "", (1, BFnone);
  `postop Uclear, "=.", "Unset", (670, BFnone); (* BFholdFirst, BFlistable, FBreadProtected *)
  (* "&", "Function", 90, BFholdAll *)
  (* "++", Increment, 660, BFholdFirst, BFreadProtected *)
  (* "--", Decrement, 660, BFholdFirst, BFreadProtected *)
  (* "!", Factorial, 610 *)
  (* "..", Repeated, 170 *)
  (* "'", "Derivative", 670, BFnHoldAll *)

  `binop Badd, "+", "Plus", (310, BFflat); (* BFflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  `binop Bsub, "-", "Subtract", (310, BFleft); (* BFleft, BFlistable, BFnumericFunction *)
  `binop Bmul, "*", "Times", (400, BFflat); (* BFflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  `binop Bdiv, "/", "Divide", (470, BFleft); (* BFleft, BFlistable, BFnumericFunction *)
  (* "^", "Power", 590, BFlistable, BFnumericFunction, BFoneIdentity *)
  (* `binop Bdiv, "%", "Mod", 470; *)
  (* ".", "Dot", 490, BFflat, BFoneIdentity *)

  (* TODO: inequalty is somehow flattened *)
  `binop Beq, "==", "Equal", (290, BFinequality); (* BFNonAssociative *)
  `binop Bneq, "!=", "Unequal", (290, BFinequality); (* BFNonAssociative *)
  `binop Blt, "<", "Less", (290, BFinequality); (* BFNonAssociative *)
  `binop Ble, "<=", "LessEqual", (290, BFinequality); (* BFNonAssociative *)
  `binop Bgt, ">", "Greater", (290, BFinequality); (* BFNonAssociative *)
  `binop Bge, ">=", "GreaterEqual", (290, BFinequality); (* BFNonAssociative *)
  (* "===", "SameQ", 290 *)
  (* "=!=", "UnsameQ", 290 *)
  `binop Band, "&&", "And", (215, BFflat); (* BFflat, BFholdAll, BFoneIdentity *)
  `binop Bor, "||", "Or", (215, BFflat); (* BFflat, BFholdAll, BFoneIdentity *)
  (* u22BB, "Xor", 215, BFflat, BFholdAll, BFoneIdentity *)

  `binop Bassign, "=", "Set", (40, BFright); (* BFright, BFholdFirst, BFsequenceHold *)
  `binop Bdelay, ":=", "SetDelayed", (40, BFright);
  (* "^=", "UpSet", 40, BFholdFirst, BFsequenceHold *)
  (* "+=", "AddTo", 100, BFright, BFholdFirst *)
  (* "-=", "SubtractFrom", 100, BFright, BFholdFirst *)
  (* "*=", "TimesBy", 100, BFright, BFholdFirst *)
  (* "/=", "DevideBy", 100, BFright, BFholdFirst *)

  (* "->", "Rule", 120, BFright, BFsequenceHold *)
  (* ":>", "RuleDelayed", 120, BFright, BFsequenceHold, BFholdRest *)
  (* "/.", "ReplaceAll", 110, BFleft *)
  (* "//.", "ReplaceRepeated", 110, BFleft *)
  (* "?", "PatternTest", BFnone, 680 *)
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
    | `preop _ -> Hashtbl.add t s (op, n, p)
    | _ -> ()) ops;
  (t: (string, someop * ident * precedence) Hashtbl.t)
end

let postop_table = begin
  let t = Hashtbl.create 17 in
  List.iter (fun (op, s, n, p) ->
    match op with
    | `postop _ -> Hashtbl.add t s (op, n, p)
    | _ -> ()) ops;
  (t: (string, someop * ident * precedence) Hashtbl.t)
end

let binop_table = begin
  let t = Hashtbl.create 17 in
  List.iter (fun (op, s, n, p) ->
    match op with
    | `binop _ -> Hashtbl.add t s (op, n, p)
    | _ -> ()) ops;
  (t: (string, someop * ident * precedence) Hashtbl.t)
end

let option_get = function
  | Some x -> x
  | None   -> raise (Invalid_argument "Option.get")

type comparison = Lt | Eq | Gt
let comp p q =
  let n = compare p q in
  if n < 0 then Lt
  else if n > 0 then Gt
  else Eq

let op_of_string_opt ts s =
  List.(ts
  |> map (fun t -> Hashtbl.find_opt t s)
  |> filter ((!=) None)
  |> (function | x::xs -> x | _ -> None))
let op_of_string ts s = op_of_string_opt ts s |> option_get
let preop_of_string = op_of_string [preop_table]
let postop_of_string = op_of_string [postop_table]
let binop_of_string = op_of_string [binop_table]
let someop_of_string = op_of_string [postop_table; preop_table; binop_table]

let parse_expr_virtual_list el =
  let mulop = EVop (op_of_string [binop_table] "*")
  and noop = EVop (op_of_string [preop_table] "\\[noop]") in
  let rec parse_to_op acc seen_binop el = match (seen_binop, el) with
    | (false, ((EVexpr _) as e)::xs) -> parse_to_op (e::mulop::acc) false xs
    | (true, ((EVexpr _) as e)::xs) -> parse_to_op (e::acc) false xs
    | (false, (EVsymbol s)::xs) -> let op = someop_of_string s in
      begin match op with
        | (`postop _, _, _) -> parse_to_op (EVop op::acc) false xs
        | (`preop _, _, _) -> parse_to_op (EVop op::mulop::acc) true xs
        | (`binop _, _, _) -> parse_to_op (EVop op::acc) true xs
      end
    | (true, (EVsymbol s)::xs) -> let op = preop_of_string s in
      begin match op with
        | (`preop _, _, _) -> parse_to_op (EVop op::acc) true xs
        | _ -> assert false
      end
    | (false, []) -> List.rev acc
    | _ -> raise (Parsing_error "extra operator") in
  let rec run cur_prec stack = function
    | (EVexpr _) as e::xs -> run cur_prec (e::stack) xs
    | (EVop (o, n, (prec, assoc))) as op::xs ->
        begin match (o, comp prec cur_prec, assoc) with
          | (`binop _, Gt, _) -> run cur_prec (op::stack) xs
          | (`binop _, Eq, BFflat) | (`binop _, Eq, BFleft) ->
              run prec (op::run_stack prec stack) xs
          | (`binop _, Lt, _) -> run prec (op::run_stack prec stack) xs
          | _ -> assert false
        end
    | [] -> run_stack 1 stack |> List.hd
    | _ -> raise (Parsing_error "extra operator")
  and run_stack new_prec = function (* there should be no postfix operator in stack*)
    | (EVexpr e)::(EVop (o, n, (prec, assoc)))::xs as stack->
        begin match (o, comp prec new_prec) with
          | (_, Lt) -> stack
          | (`preop _, Gt) -> run_stack new_prec xs
          | _ -> assert false
        end
    | _ -> assert false in
  parse_to_op [] true el |> run 0 [noop] |> (function | EVexpr e -> e | _ -> assert false)