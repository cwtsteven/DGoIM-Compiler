open SyntaxTree 
open Assembly 

let stack_b = r13 (* ret addr pointer *)
let stack_c = r15 (* code space stack potiner *)

let default_stack_size = 1024
let default_code_space = 10000000000

type s_type = { prompt: string; lambda: string; app: string; promo: string; var: string }
let s = { prompt = "$0"; lambda = "$1"; app = "$2"; promo = "$4"; var = "$5"; } 


(*
type b_type = { start: string; der: string; app_r: string; app_l: string; con: string; add_r: string; add_l: string }
let b = { start = "$0"; der = "$1"; app_r = "$2"; app_l = "$3"; con = "$4"; add_r = "$5"; add_l = "$6" }
*)

let count = ref 0 


let merge_out out_t out_u = 
	let out = Hashtbl.create 0 in
	Hashtbl.iter 
		(fun v label_list -> 
			if Hashtbl.mem out v
			then 
				Hashtbl.replace out v (List.append label_list (Hashtbl.find out v))
			else 
				Hashtbl.add out v label_list 
		) out_t;
	Hashtbl.iter 
		(fun v label_list -> 
			if Hashtbl.mem out v
			then 
				Hashtbl.replace out v (List.append label_list (Hashtbl.find out v))
			else 
				Hashtbl.add out v label_list
		) out_u; 
	out

(* runtime code *)

(* changed: eax *)
(* assumed: rax - jump target addr *)
(* return:  eax - relative displacement from s to rax *)
let calret s = 
			sub s rax
	^		sub "$5" rax

(* changed: rax, rcx, rdi, stack_c, r14 *) 
(* assumed: rax - return addr of the block, rsi - starting addr, rcx - ending addr *)
(* return:  entry addr of the copied block in rax 
let cpy = 
			sub	rsi rcx (* size *)
	^		mov stack_c rdi 
	^		mov rdi r14
	^		add rcx stack_c 
	^		rep_movsb
	^		movb "$0xe9" (mem 0 stack_c)
	^		calret stack_c
	^		mov eax (mem 1 stack_c) 
	^		add "$5" stack_c 
	^		mov r14 rax 
*)

(* changed: r14 *) 
(* assumed: rax - jmp target addr, rdi - modify target addr *)
(* return:  nothing special *)
(*
let prn = 
			mov rax r14
	^		movb "$0xe9" (mem 0 rdi)
	^		calret rdi
	^		mov eax (mem 1 rdi)
	^		mov r14 rax 
*)

let cpy = 
			sub	rsi rcx (* size *)
	^		rep_movsb


let ret node = 
		node ^ "_ret:\n"
	^		sub "$8" stack_b
	^		mov (mem 0 stack_b) rax 
	^		jmp_reg rax


(* 	
	term_to_code : term -> (label * (var, label) VarMap.t * string)
	args:
		term : SyntaxTree.term 
	return:
		(in, out, code) where in and out are the input and output interfaces 
		in : label
		out : (var, label) VarMap.t 
		code : string
*)
let rec term_to_code term = 
	match term with 
	| Var v -> 		let var = "var_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					
					let code = 
							var^"_s_in:\n"
						^		mov "$1" rax
						^		cmp "$0" rax
						^		je (var^"_long")
						^		lookup 0 rax
						^		cmp s.var rax 
						^		jne (var^"_n_dummy")
						^		pop rax
						^		pop rax
						^		pop rax
								(* internal jump *) 
						^	var^"_n_dummy:\n"
						^		lea (var^"_long_jump") rip rax 
						^		push rax 
						^		lea (var^"_s_in") rip rax 
						^		push rax 
						^		push s.var 
						^	var^"_n_out:\n"
						^		lea (var^"_s_out") rip rax
						^		mov rax (mem 0 stack_b) 
						^		add "$8" stack_b 
								(* internal jump *) 
						^	var^"_n_jmp:\n" 
						^		movabs "$0" rax
						^		jmp_reg rax
						^	var^"_n_in:\n"
								(* internal jump *)
						^	var^"_s_out:\n"
								(* external jump *) 
						^ 	ret var 

						^	var^"_long:\n"
						^		lea (var^"_long_out") rip rax
						^		mov rax (mem 0 stack_b) (* ret addr *) 
						^		add "$8" stack_b 
						^	var^"_long_jump:\n"
						^		movabs "$0" rax 
						^		jmp_reg rax
								(* internal jump *) 
						^	var^"_long_out:\n"
								(* external jump *) 
						^	ret (var^"_long") 

					in            
					(var^"_s_in", code, let vtbl = Hashtbl.create 0 in Hashtbl.add vtbl v [var^"_n_jmp"]; vtbl)

    
	| Const n -> 	let box = "box_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					let promo = "pro_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let const = "cst_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in

					let code = 
						box^"_start:\n"

						^	promo^"_s_in:\n" 
						^		lookup 0 rax
								(* internal jump *) 
						^	promo^"_n_out:\n"
						^		cmp s.var rax 
						^		jne (promo^"_n_out_dummy")
						^		pop rax
						^		pop rax
						^		pop rax
								(* no rewrites atm *)
								(* rewrite for !-C *)
						^	promo^"_n_out_dummy:\n" 
								(* external jump *) 

						^	const^"_s_in:\n" 
						^		pop rax
						^		push ("$"^(string_of_int n)) 
								(* internal jump *) 
						^	const^"_s_out:\n"
								(* external jump *)

						^	promo^"_n_in:\n"
								(* internal jump *)
						^	promo^"_s_out:\n"
						^	box^"_end:\n"
						^	ret box 
					in
					(box^"_start", code, Hashtbl.create 0) 


	| Abs (v, t) -> let box = "box_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					let promo = "pro_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let abs = "abs_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in

					let (in_t, code_t, vtbl) = term_to_code t in 
					
					let code = 
						box^"_start:\n"
						^	mov "$1" rax
						^	cmp "$0" rax
						^	je (abs^"_t")

						^	promo^"_s_in:\n" 
								(* internal jump *) 
						^	promo^"_n_out:\n"
						^ 		lookup 0 rax 
						^	promo^"_n_out_con:\n"
						^		cmp s.var rax 
						^		jne (promo^"_n_out_dummy")
						^		pop rax (* var in stack *)
						^		pop rdi (* var_s_in *)
						^		lea (abs^"_modify") rip rsi 
						^		lea (abs^"_modify_end") rip rcx 
						^		cpy 
						^		lea (box^"_start") rip rsi 
						^		lea (box^"_ret_end") rip rcx 
						^		sub rsi rcx (* size *)
						^		mov rcx rax (* remember the size *)
						^		mov stack_c rdi 
						^		rep_movsb 
						^		pop rdi 
						^		mov stack_c (mem 2 rdi)
						^		mov stack_c rdi 
						^ 		add rax stack_c
						^		jmp_reg rdi  
								(* rewrite for !-C *)
						^	promo^"_n_out_dummy:\n"
								(* external jump *) 

						^	abs^"_s_in:\n"
						^		cmp s.app rax 
						^		je (abs^"_r_out") 
						^		pop rax 
						^		push s.lambda 
						^	abs^"_s_out:\n"
								(* external jump *)
						^	promo^"_n_in:\n"
								(* internal jump *)
						^	promo^"_s_out:\n" 
						^		jmp (box^"_end") 

						^	abs^"_r_out:\n"
								(* rewrite for lambda *)
								(* connect horizontal line *)
						^		pop rax (* app *)
						^		pop rax (* arg addr *)
						^		(let rec modify_all_var ls = 
									match ls with
									| [] -> ""
									| x :: xs -> 
										lea x rip rdi 
						^				mov rax (mem 2 rdi)
						^				modify_all_var xs 
								in modify_all_var (Hashtbl.find vtbl v)
								)
								(* connect vertical line *)
						^		pop rax (* entry address *) 
						^		lea (abs^"_modify") rip rsi 
						^		lea (abs^"_modify_end") rip rcx 
						^		mov rax rdi 
						^		cpy 
						^		lea (abs^"_modify") rip rsi 
						^		lea (abs^"_modify_end") rip rcx 
						^		lea (box^"_start") rip rdi 
						^		cpy 
								(* external jump *) 
						^	abs^"_t:\n"
						^		lea (box^"_end") rip rax
						^		mov rax (mem 0 stack_b)
						^		add "$8" stack_b
						^	code_t 

						^	abs^"_modify:\n"
						^		mov "$0" rax 
						^	abs^"_modify_end:\n"
						^	box^"_end:\n"
						^ 	ret box
						^	box^"_ret_end:\n"

					in
					(box^"_start", code, (Hashtbl.remove vtbl v; vtbl)) 


	| App (t, u) ->	let app = "app_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let der = "der_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in

					let (in_t, code_t, vtbl_t) = term_to_code t in
					let (in_u, code_u, vtbl_u) = term_to_code u in

					let code = 
							app^"_s_in:\n" 
						^		mov "$1" rax
						^		cmp "$0" rax 
						^		je (app^"_t")
								(* internal jump *)
						^	app^"_r_out:\n"	
								(* internal jump *)
						^	app^"_r_jmp:\n"
						^		lea (app^"_r_in") rip rax
						^		mov rax (mem 0 stack_b) 
						^		add "$8" stack_b 
						^		push s.prompt
								(* external jump *) 
						^	code_u 

						^	app^"_r_in:\n"
						^		pop rax 
								(* internal jump *)
						^ 	app^"_l_out:\n"
						^		lea (app^"_s_in") rip rax  	(* entry addr *)
						^		push rax
						^		lea in_u rip rax 			(* arg addr *)
						^		push rax 
						^		push s.app
								(* external jump *)

							(* code for D-node *)				
						^	der^"_s_in:\n"					
								(* internal jump *)
						^	der^"_n_out:\n"	
								(* external jump *)
						^	app^"_t:\n"
						^	code_t 

					in
					(app^"_s_in", code, merge_out vtbl_t vtbl_u)

	| Add (t, u) -> let addi = "add_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let box = "box_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					let promo = "pro_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let const = "cst_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in

					let (in_t, code_t, vtbl_t) = term_to_code t in
					let (in_u, code_u, vtbl_u) = term_to_code u in

					let code = 
							addi^"_s_in:\n" 
						^		mov "$1" rax
						^		cmp "$0" rax
						^		je (box^"_start")
								(* internal jump *)
						^	addi^"_r_out:\n"	
								(* external jump *) 
						^		lea (addi^"_r_in") rip rax
						^		mov rax (mem 0 stack_b)
						^		add "$8" stack_b
						^		push s.prompt
						^	code_u 

						^	addi^"_r_in:\n"
						^		push s.prompt
								(* internal jump *)
						^ 	addi^"_l_out:\n"
						^		lea (addi^"_l_in") rip rax
						^		mov rax (mem 0 stack_b)
						^		add "$8" stack_b

						^	code_t 

						^	addi^"_l_in:\n"
						^		pop rax
						^		pop rdi
						^		pop rcx
						^		add rdi rax 
						^		push rax 
						
						^		lea (const^"_payload") rip rdi
						^		mov rax (mem 2 rdi) (* modify the content *)

						^		lea (addi^"_modify") rip rsi 
						^		lea (addi^"_modify_end") rip rcx 
						^		lea (addi^"_s_in") rip rdi 
						^		cpy 
						^		jmp (box^"_end") 

						^	addi^"_modify:\n"
						^		mov "$0" rax 
						^	addi^"_modify_end:\n"

						^	box^"_start:\n"

						^	promo^"_s_in:\n" 
						^		lookup 0 rax
								(* internal jump *) 
						^	promo^"_n_out:\n"
						^		cmp s.var rax 
						^		jne (promo^"_n_out_dummy")
						^		pop rax
						^		pop rax
						^		pop rax
								(* rewrite for !-C *)
						^	promo^"_n_out_dummy:\n"
								(* external jump *) 

						^	const^"_s_in:\n" 
						^		pop rax
						^	const^"_payload:\n" 
						^ 		movabs "$0" rax
						^		push rax
								(* internal jump *) 
						^	const^"_s_out:\n"
								(* external jump *)

						^	promo^"_n_in:\n"
								(* internal jump *)
						^	promo^"_s_out:\n"
						^	box^"_end:\n"
						^	ret box 
					in
					(addi^"_s_in", code, merge_out vtbl_t vtbl_u)


let fun_prefix = 
		"\t\tpush\t\t%rbp\n"
	^	"\t\tmov\t\t%rsp, %rbp\n"

let fun_postfix = 
		"\t\tpop\t\t%rbp\n"
	^	"\t\tret\n"

let prefix = 
			"\t\t.data\n" 
	^ 	"int.str:\n" 
	^ 		"\t\t.string\t\t\"%ld\\n\\0\"\n" 
	^ 	"hex.str:\n" 
	^ 		"\t\t.string\t\t\"%#08x\\n\\0\"\n" 
	^ 	"str.str:\n" 
	^ 		"\t\t.string\t\t\"%s\\n\\0\"\n"

let error_exit = 
		"error:\n"

let generate term = 
		print_string ((string_of_term term)^"\n");
		prefix ^ "\n"
	^		"\t\t.globl\t\t_main\n"
	^	"\n"
	^		"\t\t.text\n"
	^	"_main:\n"
	^		fun_prefix
	^		"\n"
	(*
	^		"\t\t# change the access permission of section .text to rwx\n"
	^		"\t\tlea\t\t_main(%rip), %rdi\n"
	^		"\t\tmov\t\t%rdi, %rbx\n"
	^		"\t\tcall\t\t_getpagesize\n"
	^		"\t\tmov\t\t%rax, %rsi\n"
	^		"\t\txor\t\t%r14d, %r14d\t\t# not sure why is this required\n" 
	^		"\t\txor\t\t%rdx, %rdx\n"
	^		"\t\tmov\t\t%rbx, %rax\n"
	^		"\t\tdiv\t\t%rsi\n"
	^		"\t\tsub\t\t%rdx, %rbx\n" 
	^		"\t\tlea\t\t_main(%rip), %rcx\n"
	^		"\t\tlea\t\t_end(%rip), %rsi\n"
	^		"\t\tsub\t\t%rcx, %rsi\n"
	^		"\t\tadd\t\t%rdx, %rsi\n"
	^		"\t\tmov\t\t$7, %edx\n"
	^		"\t\tmov\t\t%rbx, %rdi\n"
	^		"\t\tcall\t\t_mprotect\n"
	^		"\n" 
	*)
	^		"\t\t# allocate more code space in the heap\n"
	^		mov ("$"^string_of_int default_code_space) rdi
	^		"\t\tcall\t\t_malloc\n" 
	^		mov rax stack_c
	^		"\t\t# change the access permission of extra code space to rwx\n"
	^		"\t\tmov\t\t%rax, %rdi\n" 
	^		"\t\tmov\t\t%rdi, %rbx\n"
	^		"\t\tcall\t\t_getpagesize\n"
	^		"\t\tmov\t\t%rax, %rsi\n"
	(*^		"\t\txor\t\t%r14d, %r14d\t\t# not sure why is this required\n"*)
	^		"\t\txor\t\t%rdx, %rdx\n"
	^		"\t\tmov\t\t%rbx, %rax\n"
	^		"\t\tdiv\t\t%rsi\n"
	^		"\t\tsub\t\t%rdx, %rbx\n"
	^		mov ("$"^string_of_int default_code_space) rsi
	^		"\t\tadd\t\t%rdx, %rsi\n"
	^		"\t\tmov\t\t$7, %edx\n"
	^		"\t\tmov\t\t%rbx, %rdi\n"
	^		"\t\tcall\t\t_mprotect\n"
	^		"\n"

	^		"\t\t# allocate more space in the heap for B-stack\n"
	^		mov ("$"^string_of_int default_stack_size) rdi
	^		"\t\tcall\t\t_malloc\n" 
	^		mov rax stack_b
	^		"\n"

	^		lea "graph_start" rip rsi 
	^		lea "graph_end" rip rcx 
	^		mov stack_c rdi 
	^		sub	rsi rcx
	^		mov rcx rax
	^		rep_movsb
	^		push stack_c
	^		add rax stack_c
	^		jmp "code_space_entry"
	^		"\n"

	^	(let (in_t, code_t, ret_t) = term_to_code term in 
		"graph_start:\n"
	^	"start_out:\n"
	^		push s.prompt 
	^		lea "start_in" rip rax
	^		mov rax (mem 0 stack_b) 
	^		add "$8" stack_b
			(* external jump *)
	^	code_t
	^	"start_in:\n"
	^		pop r14 
	^		pop rax 
	^		push r14
	^		jmp_reg rax 
	^	"graph_end:\n"
		)
	^	"\n"
	^	"code_space_entry:\n"
	^		pop rdi 
	^		lea "exit" rip rax 
	^		push rax
	^		jmp_reg rdi 
	^	"\n"
	^	"exit:\n" 
	^		pop rsi
	^		lea	"int.str" rip rdi
	^		"\t\tcall _printf\n"
	^	fun_postfix
	^	"\n"

	^	error_exit
	^	"\n"
	^	"_end:\n"

