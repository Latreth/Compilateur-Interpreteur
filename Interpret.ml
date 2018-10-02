open Expr

let tamp = ref 0 ;;

let empile (p,q,r) pile = (p,q,r)::pile;;

let pop pile = match pile with 
	|[] -> failwith "pile d'environnement vide"
	|(p,q,r)::q2 -> (p,q,r,q2);;

(* Interpreteur arithmétique, qui s'occupe des fonctions arithmétiques élémentaires *)

let rec interpret_arith f e pile = match f with
	|Expr(l) -> interpreteur e l pile
	|Negcst(i) -> Int(-i)
	|Cst(i) -> Int(i)
	|Negexpr(l) -> let r = interpreteur e l pile in begin match r with 
			|Int(i) -> Int(-i)
			|Exception -> Exception
			| _ -> failwith "On ne peut faire la négation d'une expression non entière"end
	|PrInt(l) -> let a = interpret_arith l e pile in begin match a with 
			|Int(i) -> begin print_int(i);print_newline(); Int(i);  end 
			|Exception -> Exception
			| _ -> failwith "erreur pas un Int à afficher"end 
	|Add(x,y) -> let i1 = (interpret_arith x e pile) in let i2 = (interpret_arith y e pile) in begin 
			match i1,i2 with 
				|Int(i),Int(j) -> Int(i+j)
				|Exception,_-> Exception
				|_,Exception -> Exception
				|_ , _-> failwith "erreur pas un Int à ajouter" end
	|Sub(x,y) -> let i1 = (interpret_arith x e pile) in let i2 = (interpret_arith y e pile) in begin
			match i1,i2 with 
				|Int(i),Int(j) -> Int(i-j)
				|Exception,_-> Exception
				|_,Exception -> Exception
				|_ , _-> failwith "erreur pas un Int à soustraire "end
	|Mul(x,y) -> let i1 = interpret_arith x e pile in let i2 = (interpret_arith y e pile) in begin
			match i1,i2 with 
				|Int(i),Int(j) -> Int(i*j) 
				|Exception,_-> Exception
				|_,Exception -> Exception
				|_ , _ -> failwith "erreur pas un Int à multiplier"end
	|Ident(s) -> trouve s e
(* Le type Appli permet de faire le lien avec le type expr ici. On doit donc matcher les types que l'on peut retrouver dans le parser. *)
	|Appli(expr) -> begin match expr with 
				|Op(Ident(s)) -> interpret_arith (Ident(s)) e pile
				|App(expr1,expr2) -> interpreteur e expr pile	
				|Function(s,expr) -> Fonction(s,expr,e) 
				|_ -> failwith "Erreur de typage" end
	|Raise(valeur) -> begin 
				tamp := valeur;
				Exception;
				end

(* Interpreteur des conditions booléennes, surtout utiles pour les IfThenElse *)

and interpret_cond f env pile = match f with 
	|SInf(x,y) -> let i1=interpret_arith x env pile in let i2 = interpret_arith y env pile in begin
				match i1,i2 with 
					|Int(x),Int(y)-> if x<y then Int(1) else Int(0) 
					|_ , _ -> failwith "pas des Int à comparer" end
	|SSup(x,y) ->  let i1=interpret_arith x env pile in let i2 = interpret_arith y env pile in begin
				match i1,i2 with 
					|Int(x),Int(y)-> if x>y then Int(1) else Int(0) 
					|_ , _ -> failwith "pas des Int à comparer" end
	|Inf(x,y) ->  let i1=interpret_arith x env pile in let i2 = interpret_arith y env pile in begin
				match i1,i2 with 
					|Int(x),Int(y)-> if x<=y then Int(1) else Int(0) 
					|_ , _ -> failwith "pas des Int à comparer" end
	|Sup(x,y) ->  let i1=interpret_arith x env pile in let i2 = interpret_arith y env pile in begin
				match i1,i2 with 
					|Int(x),Int(y)-> if x>=y then Int(1) else Int(0) 
					|_ , _ -> failwith "pas des Int à comparer" end
	|Equ(x,y) -> let i1=interpret_arith x env pile in let i2 = interpret_arith y env pile in begin
				match i1,i2 with 
					|Int(x),Int(y)-> if x=y then Int(1) else Int(0) 
					|_ , _ -> failwith "pas des Int à comparer" end
	|Dif(x,y) ->  let i1=interpret_arith x env pile in let i2 = interpret_arith y env pile in begin
				match i1,i2 with 

					|Int(x),Int(y)-> if x<>y then Int(1) else Int(0) 
					|_ , _ -> failwith "pas des Int à comparer" end

(* Cette fonction va trouver dans la l'environnement ce qu'il faut rendre en sortie de la fonction. *)

and trouve s env = match env with 
	|Vide -> failwith "empty"
	|Var(x,v,l) -> if x=s then Int(v) else trouve s l
	|Fun(sp, arg, corps, cloture, suite) -> if sp=s then Fonction(arg, corps, cloture) else trouve s suite
	|FunRec(sp, arg, corps, cloture, suite) -> if sp=s then FonctionRec(arg, corps, cloture) else trouve s suite
	|Ref(sp,v,suite) -> if sp=s then Reference(v) else trouve s suite

(* Voici l'interpreteur principal, qui va interpreter le type expr, et faire un rendu d'un type value afin d'éviter les problèmes de typage que l'on aurait pu rencontrer. *)

and interpreteur env expr pile = match expr with 
	|Op(f) -> interpret_arith f env pile
	|If(c,l) -> let r = interpret_cond c env pile in if r=Int(1) then interpreteur env l pile else Int(0)
	|Ifelse(c,l1,l2) -> let r = interpret_cond c env pile in if r=Int(1) then interpreteur env l1 pile else interpreteur env l2 pile

	|App(e1,e2) -> let h = interpreteur env e2 pile in let r = interpreteur env e1 pile in begin match r with 
			|Fonction(s,expr2,cloture) -> begin
				match h with
					|Fonction(argh,exprh,envih)-> interpreteur (Fun(s,argh,exprh, envih,cloture)) expr2 pile 
					|Int(x)-> interpreteur (Var(s,x,cloture)) expr2 pile
					|FonctionRec(argh,exprh,envih)->interpreteur (FunRec(s,argh,exprh,envih,cloture)) expr2 pile 
					|Exception -> Exception
					|Reference(v) -> interpreteur (Ref(s,v,cloture)) expr2 pile
					end
			|FonctionRec(s,expr2,cloture) -> let nf = begin match e1 with 
									|Op(Ident(nom))->nom 
									|_ -> failwith "not an Op" end in
			begin
				match h with 
					|Fonction(argh,exprh,envih) -> interpreteur (Fun(s,argh,exprh,envih,FunRec(nf,s,expr2,cloture,cloture))) expr2 pile
					|FonctionRec(argh,exprh,envih) -> interpreteur (FunRec(s,argh,exprh,envih,FunRec(nf,s,expr2,cloture,cloture))) expr2 pile
					|Int(x) -> interpreteur (Var(s,x,FunRec(nf,s,expr2,cloture,cloture))) expr2 pile
					|Exception -> Exception
					|Reference(v) -> interpreteur (Ref(s,v,cloture)) expr2 pile
					end
			|_ -> failwith "Ceci n'est pas une fonction" end
	|Decla(s,e1,e2) -> let r = interpreteur env e1 pile in begin
		match r with
		|Int(i)-> interpreteur (Var(s,i,env)) e2 pile
		|Fonction(arg1,expr1,envir)-> interpreteur (Fun(s,arg1,expr1,envir,env)) e2 pile
		|FonctionRec(arg1,expr1,envir) -> interpreteur (FunRec(s,arg1,expr1,envir,env)) e2 pile
		|Exception -> Exception 
		|Reference(v) -> interpreteur (Ref(s,v,env)) e2 pile
		end
	|Declarec(s,e1,e2) -> let r = interpreteur env e1 pile in begin 
				match r with
				|Int(x) -> interpreteur (Var(s,x,env)) e2 pile
				|Fonction(argh,exprh,envih) -> interpreteur (FunRec(s,argh,exprh,envih,env)) e2 pile
				|FonctionRec(argh,exprh,envih) -> interpreteur (FunRec(s,argh,exprh,envih,env)) e2 pile
				|Exception -> Exception 
				|Reference(v) -> interpreteur (Ref(s,v,env)) e2 pile 
				end
	|Function(s,expr2) -> Fonction(s,expr2,env) 
	|Refe(e1) -> let r = interpreteur env e1 pile in begin match r with 
						|Exception -> Exception
						|_ -> Reference(ref r) end
	|Modifref(e2,e1) -> let r = interpreteur env e2 pile in let h = interpreteur env e1 pile in begin match r with
					|Reference(v) -> v:=h; !v
					|Exception -> Exception
					|_ -> failwith "pas une reference" end
	|Valref(e1) -> let r = interpreteur env e1 pile in begin match r with 
				|Reference(v) -> !v
				|Exception -> Exception
				|_ -> failwith "pas une reference" end
	|Trywith(expr1,s,expr3) -> let r = interpreteur env expr1 (empile (env,s,expr3) pile) in
					begin match r with 
						|Exception -> interpreteur (Var(s,!tamp,env)) expr3 pile
						|_ -> r
					end;;

