open Compiler
open ErrorHandling
open CodeGeneration

let rec read_to_empty buf =
	let s = read_line () in
		if s = "" then buf
		else (Buffer.add_string buf s; Buffer.add_string buf "\n"; read_to_empty buf)

let read_file filename buf = 
	let file = open_in filename in
	try
		while true do
			buf := !buf ^ "\n" ^ input_line file
		done
	with
	| End_of_file -> close_in file

let write_file filename content = 
	let file = open_out filename in
	Printf.fprintf file "%s" content;
	close_out file


let () = 	
	let opflag = ref true
	and sourcefile = ref ""
	and content = ref "" in
	for i = 1 to Array.length Sys.argv - 1 do
		match Sys.argv.(i) with
		| "-fopoff"	-> 	opflag := false
		| x 		-> 	sourcefile := x
	done;
	(if !sourcefile = "" then (content := Buffer.contents (read_to_empty (Buffer.create 1)); sourcefile := "test.txt")
	else read_file !sourcefile content); 
	let desfil = String.sub !sourcefile 0 ((String.length !sourcefile) - 4) ^ ".s" in
	try write_file desfil (compile !opflag !content) with
	| SyntaxError msg		->	prerr_string msg; exit(1)
	| ParserError msg 		-> 	prerr_string msg; exit(1)
	(*| UnboundVarError msg 	->	prerr_string msg; exit(1)
	| TypeError msg 		-> 	prerr_string msg; exit(1)
	| NotYetDeveloped 		-> 	prerr_string "This part is not ready yet..."; exit(1)
	*)


(*
open CodeGeneration

let rec read_to_empty buf =
	let s = read_line () in
		if s = "" then buf
		else (Buffer.add_string buf s; Buffer.add_string buf "\n"; read_to_empty buf)

let read_file filename buf = 
	let file = open_in filename in
	try
		while true do
			buf := !buf ^ "\n" ^ input_line file
		done
	with
	| End_of_file -> close_in file

let write_file filename content = 
	let file = open_out filename in
	Printf.fprintf file "%s" content;
	close_out file

let test expr = 
	let desfil = "test.s" in
	write_file desfil (generate expr)


let () = 	
	let opflag = ref true
	and sourcefile = ref ""
	and content = ref "" in
	for i = 1 to Array.length Sys.argv - 1 do
		match Sys.argv.(i) with
		| "-fopoff"	-> 	opflag := false
		| x 		-> 	sourcefile := x
	done;
	(if !sourcefile = "" then (content := Buffer.contents (read_to_empty (Buffer.create 1)); sourcefile := "test.txt")
	else read_file !sourcefile content);
	let desfil = String.sub !sourcefile 0 ((String.length !sourcefile) - 4) ^ ".s" in
	try write_file desfil (compile !opflag !content) with
	| SyntaxError msg		->	prerr_string msg; exit(1)
	| ParserError msg 		-> 	prerr_string msg; exit(1)
	| UnboundVarError msg 	->	prerr_string msg; exit(1)
	| TypeError msg 		-> 	prerr_string msg; exit(1)
	| NotYetDeveloped 		-> 	prerr_string "This part is not ready yet..."; exit(1)
*)

(*
let () =
	read_to_empty (Buffer.create 1)
	|> Buffer.contents
	|> parse_with_op
	|> SyntaxTree.string_of_program
	|> print_string;
*)

(*
let () =
	read_to_empty (Buffer.create 1)
	|> Buffer.contents
	|> parse_without_op
	|> SyntaxTree.string_of_program
	|> print_string;
*)