open Expr
open Compil

let pop pile = match pile with 
	|[] -> failwith "pile vide"
	|t::q -> (t,q);;

let push pile t = t::pile;;

let empty pile = match pile with 
	|[] -> true 
	|_ -> false;;


let rec machine pile liste = match liste with 
	(* Exécution de la machine à pile *)
	|[] -> let t1,q1 = pop pile in print_int t1
	|t::q -> begin match t with
			|C k -> machine (push pile k) q
			|P -> let t1,q1 = pop pile in print_int t1; print_newline(); machine pile q
			|A -> let t1,q1 = pop pile in let t2,q2 = pop q1 in machine (push q2 (t1+t2)) q
			|S -> let t1,q1 = pop pile in let t2,q2 = pop q1 in machine (push q2 (t1-t2)) q
			|S1 -> let t1,q1 = pop pile in machine (push q1 (-t1)) q
			|M -> let t1,q1 = pop pile in let t2,q2 = pop q1 in machine (push q2 (t1*t2)) q end ;;	


