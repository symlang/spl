open Ast

exception Error of string

module Ctx = struct
  type t = (Ast.ident, Ast.expr) Hashtbl.t
  let create () = Hashtbl.create 17
  let push = Hashtbl.add
  let get t x =
    match Hashtbl.find_opt t x with
    | Some v -> v
    | None -> Eident x
  let clear = Hashtbl.remove
end

let normalize_pass e = e
and bind_pass ~ctx e = e
and dump_pass e = Format.asprintf "%a" Dump.dump_expr e
and print_pass ~oc e = Format.fprintf oc "%a" Dump.dump_expr e; Some e

let eval_expr ~ctx e = e |> normalize_pass |> bind_pass ~ctx

let rec stmt ~ctx s =
  match s with
  | Sprint e -> eval_expr ~ctx e |> fun e -> Some e
  | Seval e -> eval_expr ~ctx e |> fun _ -> None
  | Sclear id -> None

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None::tl -> deopt acc tl
    | Some x::tl -> deopt (x::acc) tl
  in
  deopt [] l

let rec dump_list oc ~sep = function
  | x::[] -> Format.fprintf oc "%s" x
  | x::((_::_) as xs) -> Format.fprintf oc "%s%s" x sep; dump_list oc ~sep xs
  | _ -> ()

let file ?(oc=Format.std_formatter) ?(ctx = Ctx.create ()) f = List.(
  f |> map (stmt ~ctx)
    |> deoptionalize
    |> map dump_pass
    |> Format.fprintf oc "[%a]" (dump_list ~sep:",\n"))
