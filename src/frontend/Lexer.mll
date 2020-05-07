{
  open Parser

  let incr_linenum lexbuf = 
	let pos = lexbuf.Lexing.lex_curr_p in
	lexbuf.Lexing.lex_curr_p <- { pos with
		Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
		Lexing.pos_bol = pos.Lexing.pos_cnum;
	}
	;;
}

let assign = '='
let add = '+'
let sub = '-'
let mul = '*'
let div = '/'
let mod = '%'
let eq = "=="
let neq = "!="
let gt = '>'
let geq = ">="
let lt = '<'
let leq = "<="
let logical_and = "&&"
let logical_or = "||"
let logical_not = '!'


let func = "fun"
let arrow = "->"
let _let = "let"
let bind = "="
let _in = "in"
let if = "if"
let then = "then"
let else = "else"

let int = ['0'-'9']['0'-'9']*
let real = ['0'-'9']+'.'['0'-'9']+
let char = "'"_"'"
let string = '"'([^'\"']*)'"'
let true = "true"
let false = "false"

let identifier = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let l_bracket = '('[' ' '\t']*
let r_bracket = [' ' '\t']*')'

let space = [' ' '\t']+
let eol = ('\r' | '\n' | "\r\n")

rule read = parse
| add { ADD }
| sub { SUB }
| mul { MUL }
| div { DIV }
| mod { MOD }
| eq { EQ }
| neq { NEQ }
| gt { GT }
| geq { GEQ }
| lt { LT }
| leq { LEQ }
| logical_and { AND }
| logical_or { OR }
| logical_not { NOT }

| func { FUN }
| arrow { ARROW }
| _let { LET }
| bind { BIND }
| _in { IN }
| if { IF }
| then { THEN }
| else { ELSE }

| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| real { REAL (float_of_string (Lexing.lexeme lexbuf)) }
| char { CHAR (Lexing.lexeme_char lexbuf 1) }
| string { STRING (String.sub (Lexing.lexeme lexbuf) 1 ((String.length (Lexing.lexeme lexbuf)) - 2)) }
| true { BOOL true }
| false { BOOL false }

| identifier { IDENTIFIER (Lexing.lexeme lexbuf) }

| l_bracket { L_BRACKET }
| r_bracket { R_BRACKET }

| space { SPACE }
| eol { incr_linenum lexbuf; read lexbuf }

| eof { EOF }
| _ { raise (ErrorHandling.SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
