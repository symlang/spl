
(* Abstract syntax trees of Mini-Python *)

type ident = string

type preop =
  | Uneg (* -e *)

type sufop = | Uoutput

type binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)
  | Bassign | Bdelay                    (* = := *)

type constant =
  | Cnone
  | Cbool of bool
  | Cstring of string
  | Cint of int

type symbol =
  | Cident of ident
  | Cbinop of binop
  | Cprefix of preop
  | Csuffix of sufop

type expr =
  | Ecst of constant
  | Esymbol of symbol
  | Ecall of expr * expr list
  | Elist of expr list
  | Eblock of expr list
  | Eget of expr * expr (* e1[e2] *)

and stmt =
  | Sprint of expr
  | Seval of expr
  | Sclear of ident

and def = ident * ident list * stmt

and file = stmt list
