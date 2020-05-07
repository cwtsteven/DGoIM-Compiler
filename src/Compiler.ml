open Lexing
open SyntaxTree
open ErrorHandling
open MenhirLib.General
(*open SyntacticOptimisation*)
open CodeGeneration
module Interpreter = Parser.MenhirInterpreter

let trim_eol content = 
	String.map (fun c -> if c == '\r' || c == '\n' then ' ' else c) content

let pre_process content =
	content
	|> String.trim
	|> trim_eol


(* we manually run the parser by considering each tranisition *)
(*
let rec parse_with_inspecton lexbuf (result : SyntaxTree.term Interpreter.result) : term = 
	match result with
	| Interpreter.InputNeeded env 		-> 	let token = Lexer.read lexbuf in
						   	   				let startp = lexbuf.Lexing.lex_start_p
						   	   				and endp = lexbuf.Lexing.lex_curr_p in
						   	   				let result = Interpreter.offer result (token, startp, endp) in
						   	   				parse_with_inspecton lexbuf result (* request token from lexer whenever the parser needs one, then carry on parsing *)
    | Interpreter.Shifting _	
    | Interpreter.AboutToReduce _ 		-> 	let result = Interpreter.resume result in
    					   	   				parse_with_inspecton lexbuf result (* just carry on parsing *)
	| Interpreter.HandlingError env 	-> 	raise (ParserError (string_of_parse_error env lexbuf))(* produce meaningful error by inpsecting current state *)
  	| Interpreter.Accepted v 			-> 	v (* return the accepted parse tree *)
  	| Interpreter.Rejected 				-> 	assert false (* why????? you shd be handled by the HandlingError *)
*)
let parse content : term = 
	let content = pre_process content in
	let lexbuf = Lexing.from_string content in
	Parser.term Lexer.read lexbuf
	(*
	let lexbuf = Lexing.from_string content in
	try parse_with_inspecton lexbuf (Parser.Incremental.term()) with (* Parser.Incremental.term() creates the initial state *)
	| SyntaxError msg  ->	raise (SyntaxError (string_of_syntax_error lexbuf msg))
	*)


let parse_with_op content : term = 
	content
	|> parse
	(*|> SyntacticOptimisation.optimise*)

let parse_without_op content : term = parse content

let compile opflag content : string = 
	content
	|> (if opflag then parse_with_op else parse_without_op)
	|> CodeGeneration.generate;