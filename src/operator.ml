open Ast
open Utils

exception Parsing_error of string

type preop =
  | Unoop
  | Uneg | Uadd | UpreInc | UpreDec

type postop = | Uendop | Uclear | UpostInc | UpostDec

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)
  | Bassign | Bdelay                    (* = := *)

type someop = [ `binop of binop | `preop of preop | `postop of postop ]
(* TODO: what's difference between Gnonassoc and Gnone *)
type association = Gleft | Gright | Gnonassoc | Gflat | Ginequality | Gnone
type precedence = (int * association)

type vexpr =
  | EVexpr of expr
  | EVsymbol of string
  | EVop of (someop * ident * precedence)

(* TODO: association of the same precedence should be the same *)
let ops = [
  `preop Unoop, "\\[noop]", "", (0, Gnonassoc);
  `preop Uneg, "-", "Minus", (480, Gnone); (* BFlistable, BFnumericFunction *)
  `preop Uadd, "+", "Plus1", (480, Gflat); (* Gflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  `preop UpreInc, "++", "PreIncrement", (660, Gnone); (* BFholdFirst, BFreadProtected *)
  `preop UpreDec, "--", "PreDecrement", (660, Gnone); (* BFholdFirst, BFreadProtected *)
  (* "<<", "Get", 720, Gnone, BFprotected *)
  (* "!", "Not", 230 *)

  `postop Uendop, "\\[endop]", "", (1, Gnonassoc);
  `postop Uclear, "=.", "Unset", (670, Gnonassoc); (* BFholdFirst, BFlistable, FBreadProtected *)
  (* "&", "Function", 90, BFholdAll *)
  `postop UpostInc, "++", "Increment", (660, Gnone); (* BFholdFirst, BFreadProtected *)
  `postop UpostDec, "--", "Decrement", (660, Gnone); (* BFholdFirst, BFreadProtected *)
  (* "++", Increment, 660, BFholdFirst, BFreadProtected *)
  (* "--", Decrement, 660, BFholdFirst, BFreadProtected *)
  (* "!", Factorial, 610 *)
  (* "..", Repeated, 170 *)
  (* "'", "Derivative", 670, BFnHoldAll *)

  `binop Badd, "+", "Plus", (310, Gflat); (* Gflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  `binop Bsub, "-", "Subtract", (310, Gleft); (* Gleft, BFlistable, BFnumericFunction *)
  `binop Bmul, "*", "Times", (400, Gflat); (* Gflat, BFlistable, BFnumericFunction, BFoneIdentity, BForderless, BFprotected *)
  `binop Bdiv, "/", "Divide", (470, Gleft); (* Gleft, BFlistable, BFnumericFunction *)
  (* "^", "Power", 590, BFlistable, BFnumericFunction, BFoneIdentity *)
  (* `binop Bdiv, "%", "Mod", 470; *)
  (* ".", "Dot", 490, Gflat, BFoneIdentity *)

  (* TODO: inequalty is somehow flattened *)
  `binop Beq, "==", "Equal", (290, Ginequality); (* Gnonassociative *)
  `binop Bneq, "!=", "Unequal", (290, Ginequality); (* Gnonassociative *)
  `binop Blt, "<", "Less", (290, Ginequality); (* Gnonassociative *)
  `binop Ble, "<=", "LessEqual", (290, Ginequality); (* Gnonassociative *)
  `binop Bgt, ">", "Greater", (290, Ginequality); (* Gnonassociative *)
  `binop Bge, ">=", "GreaterEqual", (290, Ginequality); (* Gnonassociative *)
  (* "===", "SameQ", 290 *)
  (* "=!=", "UnsameQ", 290 *)
  `binop Band, "&&", "And", (215, Gflat); (* Gflat, BFholdAll, BFoneIdentity *)
  `binop Bor, "||", "Or", (215, Gflat); (* Gflat, BFholdAll, BFoneIdentity *)
  (* u22BB, "Xor", 215, Gflat, BFholdAll, BFoneIdentity *)

  `binop Bassign, "=", "Set", (40, Gright); (* Gright, BFholdFirst, BFsequenceHold *)
  `binop Bdelay, ":=", "SetDelayed", (40, Gright);
  (* "^=", "UpSet", 40, BFholdFirst, BFsequenceHold *)
  (* "+=", "AddTo", 100, Gright, BFholdFirst *)
  (* "-=", "SubtractFrom", 100, Gright, BFholdFirst *)
  (* "*=", "TimesBy", 100, Gright, BFholdFirst *)
  (* "/=", "DevideBy", 100, Gright, BFholdFirst *)

  (* "->", "Rule", 120, Gright, BFsequenceHold *)
  (* ":>", "RuleDelayed", 120, Gright, BFsequenceHold, BFholdRest *)
  (* "/.", "ReplaceAll", 110, Gleft *)
  (* "//.", "ReplaceRepeated", 110, Gleft *)
  (* "?", "PatternTest", Gnone, 680 *)
  (* "|", "Alternatives", 160 *)
  (* ":", "Optional", 140, Gright *)
  (* "/;", "Condition", 130, BFholdRest *)

  (* "/@", "Map", 620, Gright *)
  (* "@@", "Apply", 620, Gright *)
  (* "@@@", "ApplyLevel", 620, Gright *)
  (* uF523, "Implies", 200, Gright *)
  (* u29E6, "Equivalent", 205, BForderless *)
  (* "~~", "StringExpression", 135, Gflat, BFoneIdentity, BFprotected *)
  (* "<>", "StringJoin", 600, Gflat, BFoneIdentity *)
  (* ";;", "Span", 305 *)
  (* ";", "CompoundExpression", 10, BFholdAll, BFreadProtected *)
  (* ">>", "Put", 30 *)
  (* ">>>", "PutAppend", 30, BFprotected *)
  (* "//", "Postfix", 70, Gleft *)
  (* "@", "Prefix", 640, Gright *)
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

let op_of_string_opt ts s =
  List.(ts
  |> map (fun t -> Hashtbl.find_opt t s)
  |> filter ((!=) None)
  |> (function | x::xs -> x | _ -> None))
let op_of_string ts s = op_of_string_opt ts s |> option_get
let preop_of_string = op_of_string [preop_table]
let postop_of_string = op_of_string [postop_table]
let binop_of_string = op_of_string [binop_table]
let someop_of_string = op_of_string [postop_table; binop_table; preop_table]

let rec dump_vexpr_list oc =
  Format.fprintf oc "[%a]" (dump_alist ~sep:", " ~dump_func:dump_vexpr)
and dump_vexpr oc = function
  | EVexpr _ -> Format.fprintf oc "$"
  | EVop op -> Format.fprintf oc "%a" dump_op op
  | EVsymbol s -> Format.fprintf oc "%s" s
and dump_op oc = function
  | (`preop _, n, _) -> Format.fprintf oc "preop (%s)" n
  | (`postop _, n, _) -> Format.fprintf oc "postop (%s)" n
  | (`binop _, n, _) -> Format.fprintf oc "binop (%s)" n

let print_vexpr_list el =
  Format.eprintf "%a\n" dump_vexpr_list el; el

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
          | (`binop _, Eq, Gflat) | (`binop _, Eq, Gleft) | (`binop _, Eq, Gnone)
          | (`binop _, Lt, _) -> run prec (op::run_stack prec stack) xs
          | (`postop _, _, _) -> run prec (run_post n prec stack) xs
          | (`preop _, _, _) -> run prec (op::stack) xs
          | _ -> assert false
        end
    | [] -> run_stack 1 stack |> List.hd
    | _ -> raise (Parsing_error "extra operator")
  and run_stack new_prec = function (* there should be no postfix operator in stack*)
    | (EVexpr e)::(EVop (o, n, (prec, assoc)))::((EVop _)::_ as xs) as stack ->
        begin match (o, comp prec new_prec) with
          | (_, Lt) -> stack
          | (`preop _, Gt) -> run_stack new_prec (call n [e]::xs)
          | _ -> assert false
        end
    | (EVexpr e2)::(EVop (o, n, (prec, assoc)))::(EVexpr e1)::xs as stack ->
        begin match (o, comp prec new_prec) with
          | (_, Lt) -> stack
          | (`binop _, Gt) -> run_stack new_prec (call n [e1;e2]::xs)
          | _ -> assert false
        end
    | (EVexpr _)::(EVop _)::[] as stack -> stack
    | stack -> raise (Parsing_error (Format.asprintf "stack state error: %a" dump_vexpr_list stack))
  and run_post n prec stack =
    match run_stack prec stack with
      | (EVexpr e)::xs -> call n [e]::xs
      | _ -> assert false
  and call n el = EVexpr (Ecall (Esymbol (Cident n), el)) in
  parse_to_op [] true el (*|> print_vexpr_list*) |> run 0 [noop] |> (function | EVexpr e -> e | _ -> assert false)
