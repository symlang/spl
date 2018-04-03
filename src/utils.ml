let rec dump_list oc ~sep ~dump_func = function
  | x::[] -> Format.fprintf oc "%a" dump_func x
  | x::((_::_) as xs) -> Format.fprintf oc "%a%s" dump_func x sep; dump_list oc ~sep ~dump_func xs
  | _ -> ()

let option_get = function
  | Some x -> x
  | None   -> raise (Invalid_argument "Option.get")

type comparison = Lt | Eq | Gt
let comp p q =
  let n = compare p q in
  if n < 0 then Lt
  else if n > 0 then Gt
  else Eq