%{
open Ast
let prefix_call (op, e) = Ecall (Esymbol (Cprefix op), [e])
let binop_call (op, e1, e2) = Ecall (Esymbol (Cbinop op), [e1; e2])
%}

%token <Ast.constant> CST
%token <Ast.binop> BINOP
%token <string> IDENT
%token EOF
%token LP RP LSQ RSQ LB RB LDSQ RDSQ LDB RDB
%token COMMA SEMICOLON DOUBLESEMICOLON
%token CLEAR

/* Priority definitions and associativity of tokens */

/* Point of entry of grammar */
%start file

/* Type of values returned by the parser */
%type <Ast.file> file

%%

file:
| b = nonempty_list(stmt) EOF
    { b }
;

expr:
  l = nonempty_list(expr_factor)
    { EV l }

expr_item:
| c = CST
    { Ecst c }
| id = ident
    { Esymbol (Cident id) }
| e1 = expr_item LDSQ e2 = expr RDSQ
    { Eget (e1, e2) }
| f = expr_item LSQ e = separated_list(COMMA, expr) RSQ
    { Ecall (f, e) }
| LB l = separated_list(COMMA, expr) RB
    { Elist l }
| LDB l = separated_list(SEMICOLON, expr) RDB
    { Eblock l }
| LP e = expr RP
    { e }
;

expr_factor:
| e = expr_item  { e }
| o = binop      { Esymbol o }

stmt:
| e = expr DOUBLESEMICOLON
    { Seval e }
| e = expr SEMICOLON
    { Sprint e }
| id = ident CLEAR
    { Sclear id }
;

%inline binop:
| b = BINOP   { (Cbinop b) }
;

ident:
  id = IDENT { id }
;
