type ident = string

type t =
  | Int of Int64.t * Int64.t
  | Char of string

type id_or_t =
  | Id of ident
  | T of t

let int inf sup = Int (inf, sup)
let char charlststr = Char charlststr




















