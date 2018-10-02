type expr_arith =
	|Negcst of int
	|Cst of int
	|Ident of string
	|Add of expr_arith*expr_arith
	|Mul of expr_arith*expr_arith
	|Sub of expr_arith*expr_arith
	|PrInt of expr_arith
	|Appli of expr
	|Raise of int 
	|Expr of expr
	|Negexpr of expr

and cond =
	|SInf of expr_arith*expr_arith
	|SSup of expr_arith*expr_arith
	|Sup of expr_arith*expr_arith 	
	|Inf of expr_arith*expr_arith
	|Equ of expr_arith*expr_arith
	|Dif of expr_arith*expr_arith 

and value =
	|Int of int
	|Fonction of (string*expr*env)
	|FonctionRec of (string*expr*env)
	|Exception
	|Reference of (value ref)

and expr =
	|App of expr*expr
	|Function of (string)*expr
	|If of cond*expr
	|Ifelse of cond*expr*expr
	|Op of expr_arith
	|Decla of (string)*expr*expr
	|Declarec of (string)*expr*expr
	|Trywith of expr*(string)*expr
	|Refe of expr
	|Modifref of expr*expr
	|Valref of expr 
and env = 
	| Vide
  	| Var of string*int*env (* nom_var*val_var*suite de l'env *)
  	| Fun of string*string*expr*env*env 
	| FunRec of string*string*expr*env*env (* nom_fun*nom_argument*def_fun*cloture*suite *)
	| Ref of string*(value ref)*env;; 

let rec printlist l = match l with
	|[] -> ()
	|t :: q -> print_string(t);print_string(" "); printlist(q);;

let rec affichage_op f = match f with
	|Negcst(i) -> print_int(-i) 
	|Cst(i) -> print_int(i)
	|Add(i,j) -> print_string("(");affichage_op(i);print_string("+");affichage_op(j);print_string(")")
	|Mul(i,j) -> print_string("(");affichage_op(i);print_string("*");affichage_op(j);print_string(")")
	|Sub(i,j) -> print_string("(");affichage_op(i);print_string("-");affichage_op(j);print_string(")")
	|PrInt(l) -> print_string("prInt(");affichage_op(l);print_string(")")
	|Ident(x) -> print_string(x)
	|Appli(e) -> affichage(e)
	|Raise(i) -> print_string("raise "); print_int(i)
	|Expr(e) -> print_string("(");affichage(e);print_string(")")
	|Negexpr(e) -> print_string("-(");affichage(e);print_string(")")

and affichage_cond f = match f with 
	|SInf(i,j) -> affichage_op(i);print_string("<");affichage_op(j)
	|SSup(i,j) -> affichage_op(i);print_string (">");affichage_op(j)
	|Sup(i,j) -> affichage_op(i);print_string (">=");affichage_op(j)
	|Inf(i,j) -> affichage_op(i);print_string ("=<");affichage_op(j)
	|Equ(i,j) -> affichage_op(i);print_string ("==");affichage_op(j)
	|Dif(i,j) -> affichage_op(i);print_string ("<>");affichage_op(j)

and affichage f = match f with
	|App(x,l) -> print_string("(");affichage(x);print_string(" "); affichage l;print_string(")")
	|Function(i,e) -> print_string("(");print_string("fun "); print_string(i); print_string(" -> "); affichage(e);print_string(")")
	|Op(l) -> affichage_op l
	|If(c,l) -> print_string("if "); affichage_cond c; print_string(" then "); affichage l
	|Ifelse(c,l1,l2) -> print_string("if "); affichage_cond c; print_string(" then "); affichage l1; print_string(" else "); affichage l2
	|Decla(i,f,e) -> print_string("let "); print_string(i); print_string(" = "); affichage(f); print_string(" in ");affichage(e)
	|Declarec(i,f,e) -> print_string("let rec "); print_string(i); print_string(" = "); affichage(f); print_string(" in ");affichage(e)
	|Trywith(e1,s,e2) -> print_string("Try "); affichage(e1); print_string(" with E "); print_string (s); print_string(" -> "); affichage e2
	|Refe(e1) -> affichage(e1)
	|Modifref(e1,e2) -> affichage(e1); print_string(":="); affichage(e2)
	|Valref(e1) -> print_string("!");affichage(e1);;

