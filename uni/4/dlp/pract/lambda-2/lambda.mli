
type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
;;

val string_of_term : term -> string
;;

exception NoRuleApplies
;;

val eval : term -> term
;;

