
(* Abstract syntax trees of Mini-Python *)

type ident = string

type constant =
  | Cnone
  | Cbool of bool
  | Cstring of string
  | Cint of int

type symbol =
  | Cident of ident

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
