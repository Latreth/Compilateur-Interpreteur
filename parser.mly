%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Literal of int | And of expr*expr | Or of expr*expr *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token EOE
%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> IDENT  /* le lexème IDENT a un attribut string */
%token PLUS MINUS 
%token MULT
%token IF THEN ELSE
%token REF EXCLA ASSIGN
%token EQU NEQ SUP INF SSUP SINF
%token LPAREN RPAREN
%token LET IN PRINT REC
%token FUNCTION ARROW
%token TRY WITH E RAISE


%nonassoc EOE
%right ARROW
%nonassoc LPAREN
%nonassoc RPAREN
%right IN
%left PLUS MINUS  /* associativité gauche: a-b-c, c'est (a-b)-c, et a+b+c, c'est (a+b)+c */
%left MULT        /* associativité gauche: a*b*c, c'est (a*b)*c */
%left PRINT
%nonassoc THEN
%nonassoc ELSE 
%nonassoc LET FUNCTION EQ
%nonassoc EXCLA
%nonassoc ASSIGN


%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */

%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
/* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
   expr EOE               { $1 }  /* on veut reconnaître une "expr" */
;


expr_arith:    /* expression ayant ( du moins retournant ) une valeur numérique */
  | MINUS INT                            { Negcst $2 }
  | INT                                  { Cst $1 }
  | expr_arith PLUS expr_arith           { Add($1,$3) }
  | expr_arith MINUS expr_arith          { Sub($1,$3) }
  | expr_arith MULT expr_arith           { Mul($1,$3) }
  | PRINT expr_arith                     { PrInt($2) }
  | app1                                 { Appli($1) }
  | RAISE LPAREN E INT RPAREN            { Raise ($4) }
  | LPAREN expr RPAREN                   { Expr $2 }
  | MINUS LPAREN expr RPAREN             { Negexpr $3 }

cond:        /* expression booléenne, ne peut être utilisée que dans un if then ou un if then else */
  | LPAREN cond RPAREN                 { $2 }
  | expr_arith EQU expr_arith          { Equ($1,$3) }
  | expr_arith SUP expr_arith          { Sup($1,$3) }
  | expr_arith SSUP expr_arith         { SSup($1,$3) }
  | expr_arith INF expr_arith          { Inf($1,$3) }
  | expr_arith SINF expr_arith         { SInf($1,$3) }
  | expr_arith NEQ expr_arith          { Dif($1,$3) }


expr:       /* type le plus général, qui est aussi celui du programme entier au départ */
/* règles de grammaire pour les expressions */
  | expr_arith        	               { Op($1) }
  | IF cond THEN expr                  { If($2,$4) }
  | IF cond THEN expr ELSE expr        { Ifelse($2,$4,$6) }
  | LET IDENT EQU expr IN expr         { Decla($2,$4,$6) }
  | LET IDENT fonc2 IN expr            { Decla($2,$3,$5) }
  | FUNCTION fonc1                     { $2 }
  | LET REC IDENT EQU expr IN expr     { Declarec($3,$5,$7) }
  | LET REC IDENT fonc2 IN expr        { Declarec($3,$4,$6) }
  | TRY expr WITH E IDENT ARROW expr   { Trywith ($2,$5,$7) }
  | LET IDENT EQU REF expr IN expr     { Decla($2,Refe($5),$7) }
  | expr ASSIGN expr                   { Modifref($1,$3) }
  | EXCLA expr                         { Valref($2) }

app1:       /* application d'une fonction à des arguments, qui sont soit des variables soit des entiers (si aucun argument c'est une variable */
  | IDENT 			       { Op(Ident($1)) }
  | app1 IDENT			       { App($1,Op(Ident($2))) }
  | app1 INT 		               { App($1,Op(Cst($2))) }
  | app1 LPAREN expr RPAREN            { App($1,$3) }
  | LPAREN FUNCTION fonc1 RPAREN       { $3 }


fonc1:       /* fonctions déclarées avec fun _ -> _ */
  | IDENT fonc1                        { Function($1,$2) }
  | IDENT ARROW expr                   { Function($1,$3) }

fonc2:       /* fonctions déclarées avec let fonction variables = _ */
  | IDENT fonc2			       { Function($1,$2) }
  | IDENT EQU expr		       { Function($1,$3) }
