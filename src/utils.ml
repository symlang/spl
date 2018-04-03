let rec dump_alist oc ~sep ~dump_func = function
  | x::[] -> Format.fprintf oc "%a" dump_func x
  | x::((_::_) as xs) -> Format.fprintf oc "%a%s" dump_func x sep; dump_alist oc ~sep ~dump_func xs
  | _ -> ()

let rec dump_list oc ~sep = function
  | x::[] -> Format.fprintf oc "%s" x
  | x::xs -> Format.fprintf oc "%s%s" x sep; dump_list oc ~sep xs
  | _ -> ()

let option_get = function
  | Some x -> x
  | None   -> raise (Invalid_argument "Option.get")

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None::tl -> deopt acc tl
    | Some x::tl -> deopt (x::acc) tl
  in
  deopt [] l

type comparison = Lt | Eq | Gt
let comp p q =
  let n = compare p q in
  if n < 0 then Lt
  else if n > 0 then Gt
  else Eq