%{
	open SyntaxTree
%}

%token <int> INT
%token <float> REAL
%token <char> CHAR
%token <string> STRING
%token <bool> BOOL

%token <string> IDENTIFIER
%token AND OR NOT
%token ADD SUB MUL DIV MOD
%token EQ NEQ GT GEQ LT LEQ
%token FUN ARROW
%token LET BIND IN
%token IF THEN ELSE

%token L_BRACKET R_BRACKET
%token SPACE
%token EOL
%token EOF

%left SPACE
%left AND OR NOT
%left EQ NEQ GT GEQ LT LEQ 
%left ADD SUB
%left MUL DIV MOD

%start <SyntaxTree.term> term
%%
term:
| e = stmnt; EOF				{ e } 
;

stmnt:
| FUN; SPACE; v = IDENTIFIER; SPACE; ARROW; SPACE; e = stmnt 		{ Abs (v, e) }
| LET; SPACE; v = IDENTIFIER; SPACE; BIND; SPACE; u = expr; SPACE; 
  IN; SPACE; t = stmnt   											{ App (Abs (v, t), u) }
| e = expr 															{ e }
;

expr:
| d = data															{ d }
| t = expr; SPACE; u = expr											{ App (t, u) }
| L_BRACKET; e = stmnt; R_BRACKET									{ e }
| t = expr; SPACE; ADD; SPACE; u = expr 							{ Add (t, u) }
;

data:
| v = IDENTIFIER						{ Var v }
| i = INT 								{ Const i }
;

%inline unary_op:
| SUB			{ Neg }
| NOT 			{ Not }
;

%inline binary_op:
| ADD			{ Add }
| SUB			{ Sub }
| MUL			{ Mul }
| DIV			{ Div }
| MOD 			{ Mod }
| NEQ			{ Neq }
| EQ 			{ Eq }
| GT			{ Gt }
| GEQ			{ Geq }
| LT			{ Lt }
| LEQ			{ Leq }
| AND			{ And }
| OR			{ Or }
;
