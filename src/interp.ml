open Ast

exception Error of string

let expr e = e

let print e = Printf.printf "%a" Dump.dump_expr e; Some e

let rec stmt s =
  match s with
  | Sprint e -> Some (expr e)
  | Seval e -> ignore (expr e); None
  | Sclear id -> None

let file f = List.map stmt f |> ignore
