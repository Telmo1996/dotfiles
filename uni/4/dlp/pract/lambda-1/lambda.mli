
type term =
    TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
;;

val string_of_term : term -> string
;;

exception NoRuleApplies
;;

val eval : term -> term
;;

