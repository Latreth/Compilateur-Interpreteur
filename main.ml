open Expr
open Interpret
open Compil
open Array

(* Ces fonctions d'affichages sont présentes pour assurer un bon affichage des fonctions dans lesquels on a omis (ou fait exprès) des variables. On essaye alors de les prints "a la caml" *)
let rec affichage_op f env= match f with
	|Expr(e) -> print_string("(");affichage e env;print_string(")")
	|Negexpr(e) -> print_string("-(");affichage e env;print_string(")")
	|Negcst(i) -> print_int(-i) 
	|Cst(i) -> print_int(i)
	|Add(i,j) -> print_string("(");affichage_op i env;print_string("+");affichage_op j env;print_string(")")
	|Mul(i,j) -> print_string("(");affichage_op i env;print_string("*");affichage_op j env;print_string(")")
	|Sub(i,j) -> print_string("(");affichage_op i env;print_string("-");affichage_op j env;print_string(")")
	|Appli(e) -> affichage e env
	|PrInt(l) -> print_string("prInt(");affichage_op l env;print_string(")")
	|Raise(i) -> print_string("raise "); print_int(i)
(* C'est ici que se situe la subtilité comparée a la fonction d'affichage de Expr.ml, il faut regarder dans l'environnement si une ou plus des variables est connue, auquel cas est affiché sa valeur en lieu de son identifiant *)

	|Ident(x) -> try 
			let a = Interpret.trouve x env in 
					begin match a with 
						|Int(i) -> print_int i
						|_ -> failwith "erreur : pas entier" 
					end 
		with _ -> print_string(x)


and affichage_cond f env = match f with 
	|SInf(i,j) -> affichage_op i env ;print_string("<");affichage_op j env
	|SSup(i,j) -> affichage_op i env;print_string (">");affichage_op j env
	|Sup(i,j) -> affichage_op i env;print_string (">=");affichage_op j env
	|Inf(i,j) -> affichage_op i env;print_string ("=<");affichage_op j env
	|Equ(i,j) -> affichage_op i env;print_string ("==");affichage_op j env
	|Dif(i,j) -> affichage_op i env;print_string ("<>");affichage_op j env

and affichage f env = match f with
	|Function(i,e) -> print_string("(");print_string("fun "); print_string(i); print_string(" -> "); affichage e env;print_string(")")
	|Op(l) -> affichage_op l env
	|If(c,l) -> print_string("if "); affichage_cond c env; print_string(" then "); affichage l env
	|Ifelse(c,l1,l2) -> print_string("if "); affichage_cond c env; print_string(" then "); affichage l1 env; print_string(" else "); affichage l2 env
	|Decla(i,f,e) -> print_string("let "); print_string(i); print_string(" = "); affichage f env; print_string(" in ");affichage e env
	|Declarec(i,f,e) -> print_string("let rec "); print_string(i); print_string (" = "); affichage f env; print_string(" in "); affichage e env
	|App(x,l) -> begin match x with 
			|Op(Ident(y)) -> 
					let a = Interpret.trouve y env  in 
						begin	match a with 
							|Fonction(a,z,e) -> print_string("(");affichage x env;print_string(" "); affichage l env;print_string(")") 
							|FonctionRec(a,z,e) -> print_string ("("); affichage x env; print_string(" "); affichage l env; print_string(")")
							|_ -> print_string "vous essayer d'utiliser une variable comme une fonction"  end   
			|_ -> print_string("(");affichage x env;print_string(" "); affichage l env;print_string(")") end
	|Trywith(e1,s,e2) -> print_string("try ");affichage e1 env; print_string(" with E "); print_string(s); print_string (" -> "); affichage e2 env
	|Refe(e1) -> affichage e1 env
	|Modifref(e1,e2) -> affichage e1 env; print_string(":="); affichage e2 env
	|Valref(e1) -> print_string("!");affichage e1 env;;

let affichage_value f = match f with 
	|Int(i) -> print_int(i)
	|Fonction(s,exp,env) -> begin 
					print_string s;
					print_string "->";
					affichage exp env ;
					print_string " = <fun>";
				end
	|FonctionRec(s,exp,env) -> begin
					print_string s;
					print_string "->";
					affichage exp env ;
					print_string " = <fun rec>";
				end
	|Exception -> print_string("exception")
	|Reference(v) -> print_string("ref "); begin match !v with 
						|Int(i) -> print_int(i)
						|_ -> failwith "pas un entier" end ;;


let rec verif f = match f with
	(* Il s'agit de vérifier que l'expression parsée est arithmétique pure, pour utilisation de -interm ou -machine *)
	|Op(e) -> verif_arith e
	| _ -> 0 

and verif_arith e = match e with
	|Negcst (i) -> 1
	|Cst (i) -> 1
	|Ident (s) -> 1
	|Add (e1,e2) -> verif_arith(e1)*verif_arith(e2)
	|Mul (e1,e2) -> verif_arith(e1)*verif_arith(e2)
	|Sub (e1,e2) -> verif_arith(e1)*verif_arith(e2)
	|PrInt (e) -> verif_arith(e)
	|Appli (e) -> 0
	|Raise(i) -> 0 
	|Expr(e) -> verif e
	|Negexpr(e) -> verif e;;

let parsef file = 
	let c = try open_in file with _ -> failwith "Erreur : ce fichier n'existe pas a cet emplacement" in
	let lexbuf = Lexing.from_channel c in
	try Parser.main Lexer.token lexbuf with _ -> failwith "Erreur : syntaxe invalide"

let calc_seul () =
	(* Interpréteur seul, sur utilisation de ./fouine *)
	if Array.length Sys.argv = 1 then begin
		let lexbuf = Lexing.from_channel stdin in
        	let parse () = Parser.main Lexer.token lexbuf in
        	let result = try parse () with _ -> failwith "Erreur : syntaxe invalide" in
		affichage_value(Interpret.interpreteur Vide result []); print_newline ()
	end
	else begin
		let result = parsef Sys.argv.(1) in
		affichage_value(Interpret.interpreteur Vide result []); print_newline ()
	end ;;

let calc_debug () =
	(* Affichage puis interprétation, sur utilisation de ./fouine -debug *)
	if Array.length Sys.argv = 2 then begin
		let lexbuf = Lexing.from_channel stdin in
        	let parse () = Parser.main Lexer.token lexbuf in
        	let result = try parse () with _ -> failwith "Erreur : syntaxe invalide" in
		Expr.affichage result; print_newline (); affichage_value(Interpret.interpreteur Vide result []); print_newline ()
	end
	else begin
		let result = parsef Sys.argv.(2) in
		Expr.affichage result; print_newline (); affichage_value(Interpret.interpreteur Vide result []); print_newline ()
	end ;;
	
let calc_machine () =
	(* Exécution sur la machine à pile, sur utilisation ./fouine -machine *)
	if Array.length Sys.argv = 2 then begin
		let lexbuf = Lexing.from_channel stdin in
        	let parse () = Parser.main Lexer.token lexbuf in
        	let result = try parse () with _ -> failwith "Erreur : syntaxe invalide" in
		if (verif result = 0) then
			print_string ("Erreur : utilisation de l'option -machine sur une expression non arithmétique\n")
		else begin
			let liste = Compil.compile result in
			Machine_a_pile.machine [] liste
		end;
		print_newline ()
	end
	else begin
		let result = parsef Sys.argv.(2) in
		if (verif result = 0) then
			print_string ("Erreur : utilisation de l'option -machine sur une expression non arithmétique\n")
		else begin
			let liste = Compil.compile result in
			Machine_a_pile.machine [] liste
		end;
		print_newline ()
	end ;;
	

let rec write file : code -> unit = function
	(* Fonction permettant d'écrire le code ( liste d'instructions ) généré par le compilateur dans un fichier *)
	| [] -> output_string file "]"
	| [a] -> begin match a with
			| C(i) -> output_string file "C "; output_string file (string_of_int i); output_string file "]"
			| A -> output_string file "A"; output_string file "]"
			| M -> output_string file "M"; output_string file "]"
			| S -> output_string file "S"; output_string file "]"
			| S1 -> output_string file "S1"; output_string file "]"
			| P -> output_string file "P"; output_string file "]" end
	| t :: q -> begin match t with
			| C(i) -> output_string file "C "; output_string file (string_of_int i); output_string file "; "; write file q
			| A -> output_string file "A"; output_string file "; "; write file q
			| M -> output_string file "M"; output_string file "; "; write file q
			| S -> output_string file "S"; output_string file "; "; write file q
			| S1 -> output_string file "S1"; output_string file ";"; write file q
			| P -> output_string file "P"; output_string file "; "; write file q end;;

let calc_interm () =
	(* Ecriture du programme compilé vers la machine à pile dans le fichier indiqué, sur utilisation de ./fouine -interm. Le deuxième argument doit être l'emplacement de sauvegarde des résultats. *)
	if Array.length Sys.argv = 2 then failwith "Erreur : emplacement de sauvegarde non indiqué"
	else if Array.length Sys.argv = 3 then begin
		let lexbuf = Lexing.from_channel stdin in
        	let parse () = Parser.main Lexer.token lexbuf in
        	let result = try parse () with _ -> failwith "Erreur : syntaxe invalide" in
		if (verif result = 0) then
			print_string ("Erreur : utilisation de l'option -interm sur une expression non arithmétique\n") 
		else begin
			let liste = Compil.compile result in
			let file = open_out Sys.argv.(2) in
			output_string file "[";
			write file liste;
			close_out file;
		end
	end
	else begin
		let result = parsef Sys.argv.(3) in
		if (verif result = 0) then
			print_string ("Erreur : utilisation de l'option -interm sur une expression non arithmétique\n") 
		else begin
			let liste = Compil.compile result in
			let file = open_out Sys.argv.(2) in
			output_string file "[";
			write file liste;
			close_out file;
		end
	end ;;

let _ =
	(* On regarde si il y a une option à gérer, et on lance le calc adéquat
	   Les options doivent êtres le premier argument lors de l'exécution *)
	if Array.length Sys.argv = 1 then calc_seul()
	else begin
	let s = Sys.argv.(1) in
	  match s with
	  | "-debug" -> calc_debug ()
	  | "-machine" -> calc_machine ()
	  | "-interm" -> calc_interm ()
	  | _ -> calc_seul ()
(* print_string "Erreur : option non disponible. Usage : ./fouine ou avec options -debug, -machine ou -interm file_location.\n" *)
	end;;
