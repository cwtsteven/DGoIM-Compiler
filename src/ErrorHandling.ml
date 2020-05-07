open Lexing
open Parser
open Parser.MenhirInterpreter
open MenhirLib.General

exception SyntaxError of string
exception ParserError of string

(*

let symbol2string (type a) (symbol : xsymbol) : string =
	match symbol with
	| X (T T_INT) 			-> "int"
	| X (T T_REAL) 			-> "real"
	| X (T T_CHAR) 			-> "char"
	| X (T T_STRING) 		-> "string" 
	| X (T T_BOOL) 			-> "bool"
	| X (T T_TYPEVOID) 		-> "void"
	| X (T T_TYPEINT)		-> "int"
	| X (T T_TYPEREAL)		-> "real"
	| X (T T_TYPECHAR)		-> "char"
	| X (T T_TYPESTRING)	-> "string"
	| X (T T_TYPEBOOL)		-> "bool"
	| X (T T_IDENTIFIER)    -> "identifier"
	| X (T T_ASSIGN)		-> "="
	| X (T T_AND) 			-> "&&"
	| X (T T_OR) 			-> "||"
	| X (T T_NOT) 			-> "!"
	| X (T T_ADD) 			-> "+"
	| X (T T_SUB) 			-> "-"
	| X (T T_MUL) 			-> "*"
	| X (T T_DIV) 			-> "/"
	| X (T T_MOD) 			-> "%"
	| X (T T_EQ) 			-> "=="
	| X (T T_NEQ) 			-> "!="
	| X (T T_GT) 			-> ">"
	| X (T T_GEQ) 			-> ">="
	| X (T T_LT) 			-> "<"
	| X (T T_LEQ) 			-> "<="
	| X (T T_PROMPT) 		-> "<<"
	| X (T T_PRINT) 		-> ">>"
	| X (T T_IF) 			-> "if"
	| X (T T_ELSE) 			-> "else"
	| X (T T_DO) 			-> "do"
	| X (T T_WHILE) 		-> "while"
	| X (T T_FOR) 			-> "for"
	| X (T T_COLON) 		-> ":"
	| X (T T_BREAK)			-> "Break"
	| X (T T_CONTINUE)		-> "Continue"
	| X (T T_MAIN)			-> "main"
	| X (T T_RETURN)		-> "return"
	| X (T T_COMMA)			-> ","
	| X (T T_SEMICOLON) 	-> ";"
	| X (T T_L_BRACKET) 	-> "("
	| X (T T_R_BRACKET)		-> ")"
	| X (T T_L_CBRACKET)	-> "{"
	| X (T T_R_CBRACKET)	-> "}"
	| X (T T_EOF)			-> "eof"
	| X (N N_type_)			-> "type declaration"
	| X (N N_program)		-> "program"
	| X (N N_block)			-> "block"
	| X (N N_list_stmnt_) 	-> "i'll delete u"
	| X (N N_stmnt)			-> "statement"
	| X (N N_if_stmnt)		-> "if-statement"
	| X (N N_while_stmnt)	-> "while-statement"
	| X (N N_do_while_stmnt) -> "do-while-statement"
 	| X (N N_for_stmnt)		-> "for-statement"
	| X (N N_expr)			-> "expression"
	| X (N N_data)			-> "data"
	| X (T T_error)			-> "error"
	| X (N N_fun_call_param) -> "funtion call"
	| X (N N_list_top_level_) -> "identifier"
	| X (N N_separated_nonempty_list_COMMA_expr_) -> "expression"

	| X (N N_declare_stmnt) -> "declaration of variable"
	| X (N N_param) -> "parameter"
	| X (N N_params) -> "parameters"
	| X (N N_loption_separated_nonempty_list_COMMA_expr__) -> "why so many???????"
	| X (N N_top_level) -> "huh???"

let positionString lexbuf : string =
	let pos = lexbuf.lex_curr_p in  "Line " ^ string_of_int pos.pos_lnum ^ ", Column "  ^ string_of_int (pos.pos_cnum - pos.pos_bol)  

let string_of_syntax_error lexbuf msg : string = "Syntax Error in " ^ positionString lexbuf ^ ". " ^ msg ^ "\n"

let rec rhs_string symbols : string = 
	match symbols with
	| [] -> ""
	| (symbol :: ss) -> symbol2string symbol ^ " " ^ rhs_string ss

let rec find_symbol_in_prod symbols symbol : xsymbol option = 
	match symbols with
	| [] 
	| _ :: []		  -> None
	| s :: next :: ss -> if compare_symbols s (X symbol) == 0 then Some next else find_symbol_in_prod (next :: ss) symbol

let rec find_symbol_throung_all_prod items symbol : xsymbol option = 
	match items with 
	| [] -> None
	| (prod, _) :: is -> let symbols = rhs prod in
						 (*print_string (rhs_string symbols ^ "\n");*)
						 let target = find_symbol_in_prod symbols symbol in (
							 match target with
							 | None -> find_symbol_throung_all_prod is symbol
							 | x -> x
						 )

let rec find_symbol_through_stack stack symbol : xsymbol option = 
	match Lazy.force stack with
	| Nil -> None
	| Cons (Element (state, _, _, _), tailStack) -> 
		let items = items state in (
			match find_symbol_throung_all_prod items symbol with
			| None -> find_symbol_through_stack tailStack symbol
			| Some (X (N N_fun_call_param)) -> Some (X (T T_R_BRACKET))
			| Some (X (N N_block)) 			-> Some (X (T T_L_CBRACKET))
			| Some (X (N N_list_stmnt_))	-> Some (X (T T_R_CBRACKET))
			| Some (X (T T_COMMA))			-> Some (X (T T_R_BRACKET))
			| Some (X (N N_separated_nonempty_list_COMMA_expr_)) -> Some (X (N N_separated_nonempty_list_COMMA_expr_))
			| Some (X (N N_declare_stmnt))  -> Some (X (N N_declare_stmnt))

			| Some (X (T T_ELSE))    		-> Some (X (T T_ELSE))
			| Some (X (N N_expr)) 			-> Some (X (N N_expr))
			| Some (X (T T_COLON)) 			-> Some (X (T T_COLON))
			| Some (X (T T_SEMICOLON)) 		-> Some (X (T T_SEMICOLON))
			| Some (X (T T_L_BRACKET)) 		-> Some (X (T T_L_BRACKET))
			| Some (X (T T_R_BRACKET))		-> Some (X (T T_R_BRACKET))
			| Some (X (T T_L_CBRACKET))		-> Some (X (T T_L_CBRACKET))
			| Some (X (T T_R_CBRACKET))		-> Some (X (T T_R_CBRACKET))
			| Some (X (N N_list_top_level_))  -> Some (X (N N_list_top_level_))
			| _ 							-> find_symbol_through_stack tailStack symbol
			)
		
(* 
	printing meaningful error msg by inspecting current state 
	algorithm:
		get the last symbol we put on the stack (this symbol is still okay, not the one causing error)
		get all the prod rules
		search for the symbol
		for every rule
			if we have one, and if it is not the last symbol in the right-hand-side of the rule
				we return the next symbol <- thats the one we are expecting
			else 
				continue
		end
		if no match for every rules, we pop the previous state from stack and iterate every rules again
*)
let string_of_parse_error env lexbuf : string = 
	let stack = stack env in
	match Lazy.force stack with
		| Nil -> "Parse error in " ^ positionString lexbuf ^ ". Unexpected token: " ^ Lexing.lexeme lexbuf ^ "\n"
		| Cons (Element (state, _, _, _), _) -> 
			let symbol = incoming_symbol state in (
				(*print_string (symbol2string (X symbol) ^ "\n");*)
				match find_symbol_through_stack stack symbol with
				| None 		   -> "Parse error in " ^ positionString lexbuf ^ ". Unexpected token: " ^ Lexing.lexeme lexbuf ^ "\n"
				| Some xsymbol -> "Parse error in " ^ positionString lexbuf ^ ". " ^ symbol2string xsymbol
								 ^ " was expected but I got this token: " ^ Lexing.lexeme lexbuf ^ "\n"
				)
*)
	