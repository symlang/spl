open Ast

exception Error of string

let normalize_pass e = e
and dump_pass e = Format.asprintf "%a" Dump.dump_expr e
and print_pass e = Format.printf "%a\n" Dump.dump_expr e; Some e

let eval_expr e = e |> normalize_pass


let rec stmt s =
  match s with
  | Sprint e -> eval_expr e |> print_pass
  | Seval e -> eval_expr e |> ignore; None
  | Sclear id -> None

let file f = List.map stmt f |> ignore
