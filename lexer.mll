{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']   { token lexbuf }    (* on saute les blancs, les tabulations et les retours chariots *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | ";;"            { EOE }
  | "+"             { PLUS }
  | '-'             { MINUS }
  | '*'		    { MULT }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "fun"           { FUNCTION }
  | "->"            { ARROW }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | "if"	    { IF }
  | "then"	    { THEN }
  | "else"          { ELSE }
  | "="             { EQU }
  | "<>"            { NEQ }
  | '>'             { SSUP }
  | '<'             { SINF }
  | ">="            { SUP }
  | "<="            { INF }
  | "let"           { LET }
  | "in"            { IN }
  | "prInt"         { PRINT }
  | "rec"           { REC }
  | "E"             { E }
  | "try"           { TRY }
  | "with"          { WITH }
  | "raise"         { RAISE }
  | "ref"           { REF }
  | "!"             { EXCLA }
  | ":="            { ASSIGN }
  | ['a'-'z' '_'] ['A'-'Z' '0'-'9' 'a'-'z' '/']* as s     { IDENT(s) }
  | eof             { raise Eof } 
