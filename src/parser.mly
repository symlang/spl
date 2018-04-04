%{
open Ast
open Operator
%}

%token <Ast.constant> CST
%token <string> OPSYM IDENT
%token EOF
%token LP RP LSQ RSQ LB RB BLOCK_BEGIN BLOCK_END
%token COMMA SEMICOLON DOUBLESEMICOLON
%token CLEAR

/* Priority definitions and associativity of tokens */

/* Point of entry of grammar */
%start <Ast.file> file

%%

file:
| b = stmt+ EOF
    { b }
;

expr:
  l = expr_factor+
    { parse_expr_virtual_list l }
;

expr_item:
| c = CST
    { Ecst c }
| id = ident
    { Esymbol (Cident id) }
| e1 = expr_item LSQ LSQ e2 = expr RSQ RSQ
    { Eget (e1, e2) }
| f = expr_item LSQ e = separated_list(COMMA, expr) RSQ
    { Ecall (f, e) }
| LB l = separated_list(COMMA, expr) RB
    { Elist l }
| BLOCK_BEGIN l = separated_list(SEMICOLON, expr) BLOCK_END
    { Eblock l }
| LP e = expr RP
    { e }
;

expr_factor:
| e = expr_item  { EVexpr e }
| o = OPSYM      { EVsymbol o }

stmt:
| e = expr DOUBLESEMICOLON
    { Seval e }
| e = expr SEMICOLON
    { Sprint e }
| id = ident CLEAR
    { Sclear id }
;

ident:
  id = IDENT { id }
;
