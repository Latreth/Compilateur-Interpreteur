open Expr

type instr = C of int | A | S | M | P | S1
type code = instr list

let rec compile : expr -> code = function
	(* fonctions de compilation adaptées aux types renvoyés par le parser *)
	| Op(e) -> compile1 e
	| _ -> failwith "Expression non autorisée avec cette option"

and compile1 :  expr_arith -> code = function
	| Negcst k -> [C (-k)]
	| Cst k -> [C k]
	| Add (e1,e2) -> (compile1 e2)@(compile1 e1)@[A]
	| Sub (e1,e2) -> (compile1 e2)@(compile1 e1)@[S]
	| Mul (e1,e2) -> (compile1 e2)@(compile1 e1)@[M]
	| PrInt e -> (compile1 e)@[P]
	| Expr e -> compile e
	| Negexpr e -> (compile e)@[S1]
	| Appli e -> failwith "Erreur : application de fonction interdite avec cette option"
	| _ -> failwith "Erreur : on ne peut pas avoir un nom de variable ici";;
