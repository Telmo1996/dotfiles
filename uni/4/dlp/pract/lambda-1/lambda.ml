
type term =
    TmVar of string
  | TmAbs of string * term
  | TmApp of term * term
;;

let rec string_of_term tm = match tm with
    TmVar s -> s
  | TmAbs (s, t) -> "(lambda " ^ s ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmVar s -> [s]
  | TmAbs (s, t) -> ldif (free_vars t) [s]
  | TmApp (t1, t2) -> lunion (free_vars t1) (free_vars t2)
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
 
let rec subst x s tm = match tm with
    TmVar y ->
      if y = x then s else tm
  | TmAbs (y, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
;;

let isval tm = match tm with
    TmVar _ -> true
  | TmAbs _ -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 tm = match tm with
    (* E-AppAbs *)
    TmApp (TmAbs (x, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 t1 in
      TmApp (t1', t2)

  | _ ->
      raise NoRuleApplies
;;

let rec eval tm =
  try
    let tm' = eval1 tm in
    eval tm'
  with
    NoRuleApplies -> tm
  ;;

