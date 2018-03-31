%{
open Ast
%}

%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT
%token EOF
%token LP RP LSQ RSQ LB RB LDSQ RDSQ LDB RDB
%token COMMA SEMICOLON DOUBLESEMICOLON
%token PLUS MINUS TIMES DIV MOD AND OR ASSIGN DELAY

/* Priority definitions and associativity of tokens */

%right ASSIGN DELAY
%left OR
%left AND
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus
%nonassoc LDSQ

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
| c = CST
    { Ecst c }
| id = ident
    { Eident id }
| e1 = expr LDSQ e2 = expr RDSQ
    { Eget (e1, e2) }
| MINUS e1 = expr %prec unary_minus
    { Eunop (Uneg, e1) }
| e1 = expr o = binop e2 = expr
    { Ebinop (o, e1, e2) }
| f = ident LSQ e = separated_list(COMMA, expr) RSQ
    { Ecall (f, e) }
| LB l = separated_list(COMMA, expr) RB
    { Elist l }
| LDB l = separated_list(SEMICOLON, expr) RDB
    { Eblock l }
| LP e = expr RP
    { e }
;

stmt:
| e = expr DOUBLESEMICOLON
    { Seval e }
| e = expr SEMICOLON
    { Sprint e }
;

%inline binop:
| PLUS   { Badd }
| MINUS  { Bsub }
| TIMES  { Bmul }
| DIV    { Bdiv }
| MOD    { Bmod }
| c=CMP  { c    }
| AND    { Band }
| OR     { Bor  }
| ASSIGN { Bassign }
| DELAY  { Bdelay  }
;

ident:
  id = IDENT { id }
;
