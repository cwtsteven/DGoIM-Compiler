open SyntaxTree 
open Assembly 

let rewrite_flag = r13 
let stack_c = r15

type s_type = { prompt: string; lambda: string; app: string; promo: string; var: string }
let s = { prompt = "$0"; lambda = "$1"; app = "$2"; promo = "$4"; var = "$5" } 

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
(* return:  entry addr of the copied block in rax *)
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


(* changed: r14 *) 
(* assumed: rax - jmp target addr, rdi - modify target addr *)
(* return:  nothing special *)
let prn = 
			mov rax r14
	^		movb "$0xe9" (mem 0 rdi)
	^		calret rdi
	^		mov eax (mem 1 rdi)
	^		mov r14 rax


(* 	
	term_to_code : term -> (label * (var, label) Hashtbl.t * string)
	args:
		term : SyntaxTree.term 
	return:
		(in, out, code) where in and out are the input and output interfaces 
		in : label
		out : (var, label) Hashbl.t 
		code : string
*)
let rec term_to_code term vtbl = 
	match term with 
	| Var v -> 		let var = "var_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					
					let code = 
							var^"_s_in:\n"
								(* internal jump *) 
						^		lookup 0 rax 
						^	var^"_n_out:\n"
						^		cmp s.var rax 
						^		jne (var^"_n_jmp")
						^		lea (var^"_s_in") rip r14	(* for !-C rewrite *)
						^		push r14
						^		lea (var^"_s_out") rip r14  (* ret addr *)
						^		push r14
						^		push s.var
						^	var^"_n_jmp:\n" 
						^		lea (Hashtbl.find vtbl v) rip rax
						^		nop 
						^		nop 
						^		nop
						^	var^"_n_jmp_3:\n" 
						^		lea (var^"_n_jmp") rip rdi
						^		movw "$0x48b8" (mem 0 rdi)
						^		movq rax (mem 2 rdi)
						^		lea (var^"_n_jmp_2") rip rdi 
						^		prn 
						^		lea (var^"_n_jmp_3") rip rdi
						^		lea (var^"_n_jmp_2") rip rax
						^		prn  
						^	var^"_n_jmp_2:\n" 
						^		nop
						^		nop 
						^		nop 
						^		nop 
						^		nop 
						^	var^"_n_in:\n"
								(* internal jump *)
						^	var^"_s_out:\n"
								(* external jump *) 
					in            
					(var^"_s_in", code) 

    
	| Const n -> 	let box = "box_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					let promo = "pro_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let const = "cst_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in

					let code = 
							box^"_start:\n"

						^	promo^"_s_in:\n" 
								(* internal jump *) 
						^	promo^"_n_out:\n"
						^		lookup 0 rax
						^		cmp s.var rax 
						^		jne (promo^"_n_out_dummy") 
								(* rewrite for !-C *)
						^		pop r14 (* v *) 
						^		pop rax (* ret addr *)
						^		lea (box^"_start") rip rsi
						^		lea (box^"_end") rip rcx
						^		cpy   
						^		pop rdi (* for !-C rewrite *)
						^		prn 
						^		jmp_reg rax (* hopefully the only place needed indirect jump *) 
						^	promo^"_n_out_dummy:\n"
								(* external jump *) 

						^	const^"_s_in:\n" 
						^		pop r14
						^		push ("$"^(string_of_int n)) 
								(* internal jump *) 
						^	const^"_s_out:\n"
								(* external jump *)

						^	promo^"_n_in:\n"
								(* internal jump *)
						^	promo^"_s_out:\n"
						^	box^"_end:\n"
					in
					(box^"_start", code) 


	| Abs (v, t) -> let box = "box_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					let promo = "pro_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let abs = "abs_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					
					let (in_t, code_t) = term_to_code t (Hashtbl.add vtbl v (abs^"_l_out");vtbl) in 

					let code = 
							box^"_start:\n"

						^	promo^"_s_in:\n" 
								(* internal jump *) 
						^	promo^"_n_out:\n"
						^ 		lookup 0 rax 
						^	promo^"_n_out_con:\n"
						^		cmp s.var rax 
						^		jne (promo^"_n_out_dummy")
								(* rewrite for !-C *)
						^		pop r14 (* v *) 
						^		pop rax (* ret addr *)
						^		lea (box^"_start") rip rsi
						^		lea (box^"_end") rip rcx
						^		cpy  
						^		pop rdi 
						^		prn 
						^		jmp_reg rax (* hopefully the only place needed indirect jump *) 
						^	promo^"_n_out_dummy:\n"
								(* external jump *) 

						^	abs^"_s_in:\n"
						^		cmp s.app rax 
						^		je (abs^"_r_out") 
						^		pop r14
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
						^		pop r14 (* app *)
						^		pop rax (* arg addr *)
						^		lea (abs^"_l_out") rip rdi
						^		prn  
								(* connect vertical line *)
						^		pop rdi (* entry addr *) 
						^		lea in_t rip rax 
						^		prn
						^		push s.prompt
								(* external jump *) 

						^	code_t
						^		jmp (box^"_end") 

						^	abs^"_l_out:\n" 
						^		nop (* will be rewritten in runtime *)
						^		nop 
						^		nop 
						^		nop 
						^		nop 

						^	box^"_end:\n"

					in
					(box^"_start", code)


	| App (t, u) ->	let app = "app_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let der = "der_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in

					let (in_t, code_t) = term_to_code t vtbl in
					let (in_u, code_u) = term_to_code u vtbl in

					let code = 
							app^"_s_in:\n" 
								(* internal jump *)
						^		nop 	
						^		nop 	
						^		nop 	
						^		nop 	
						^		nop 	
						^	app^"_r_out:\n"	
								(* internal jump *)
						^	app^"_r_jmp:\n"
								(* external jump *) 
						^	code_u 

						^	app^"_r_in:\n"
						^		pop r14 
								(* internal jump *)
						^ 	app^"_l_out:\n"
						^		lea (app^"_s_in") rip r14  	(* entry addr *)
						^		push r14
						^		lea in_u rip r14 			(* arg addr *)
						^		push r14 
						^		push s.app
								(* external jump *)

							(* code for D-node *)				
						^	der^"_s_in:\n"					
								(* internal jump *)
						^	der^"_n_out:\n"	
								(* external jump *)

						^	code_t 
					in
					(app^"_s_in", code)

	| Add (t, u) -> let addi = "add_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let box = "box_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in 
					let promo = "pro_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in
					let const = "cst_" ^ string_of_int (!count) in
					let _ = count := !count + 1 in

					let (in_t, code_t) = term_to_code t vtbl in
					let (in_u, code_u) = term_to_code u vtbl in

					let code = 
							addi^"_s_in:\n" 
								(* internal jump *)
						^	addi^"_r_out:\n"	
								(* external jump *) 
						^	code_u 

						^	addi^"_r_in:\n"
						^		push s.prompt
								(* internal jump *)
						^ 	addi^"_l_out:\n"
								(* deleted external jump *)

						^	code_t 

						^	addi^"_l_in:\n"
						^		pop rax
						^		pop rdi
						^		add rdi rax 
						^		push rax 
						
						^		lea (const^"_payload") rip rdi
						^		mov eax (mem 1 rdi) (* modify the content *)

						^	lea (addi^"_s_in") rip rdi
						^	lea (box^"_start") rip rax
						^	prn
						^	jmp (box^"_end") 

						^	box^"_start:\n"

						^	promo^"_s_in:\n" 
								(* internal jump *) 
						^	promo^"_n_out:\n"
						^		lookup 0 rax
						^		cmp s.var rax 
						^		jne (promo^"_n_out_dummy") 
								(* rewrite for !-C *)
						^		pop r14 (* v *) 
						^		pop rax (* ret addr *)
						^		lea (box^"_start") rip rsi 
						^		lea (box^"_end") rip rcx
						^		cpy   
						^		pop rdi (* for !-C rewrite *)
						^		prn 
						^		jmp_reg rax (* hopefully the only place needed indirect jump *) 
						^	promo^"_n_out_dummy:\n"
								(* external jump *) 

						^	const^"_s_in:\n" 
						^		pop r14
						^	const^"_payload:\n" 
						^		push "$2147483647"
								(* internal jump *) 
						^	const^"_s_out:\n"
								(* external jump *)

						^	promo^"_n_in:\n"
								(* internal jump *)
						^	promo^"_s_out:\n"
						^	box^"_end:\n"
					in
					(addi^"_s_in", code)
		

let printf_int n  =
		"\t\tlea\t\tint.str(%rip), %rdi\n"
	^	"\t\tmov\t\t"^n^", %rsi\n"
	^	"\t\tcall\t\t_printf\n"

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

	^		"\t\t# allocate more code space in the heap\n"
	^		mov ("$"^string_of_int default_code_space) rdi
	^		"\t\tcall\t\t_malloc\n" 
	^		mov rax stack_c
	^		"\t\t# change the access permission of extra code space to rwx\n"
	^		"\t\tmov\t\t%rax, %rdi\n" 
	^		"\t\tmov\t\t%rdi, %rbx\n"
	^		"\t\tcall\t\t_getpagesize\n"
	^		"\t\tmov\t\t%rax, %rsi\n"
	^		"\t\txor\t\t%r14d, %r14d\t\t# not sure why is this required\n" 
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

	^	(let (in_t, code_t) = term_to_code term (Hashtbl.create 0) in 
		"graph_start:\n"
	^		"\t\t# initialise stack pointers and flag, rsp=stack, r15=code_stack\n"
	^		"\n"
	^		"\t\t# initialise the machine state\n"
	^		"\n"
	^	"start_out:\n"
	^		push s.prompt 
			(* external jump *)
	^	code_t
	^	"start_in:\n"
	^		pop rax 
	^		printf_int rax 
	^	"graph_end:\n"
		)
	(*
	^	lea "exit" rip rax
	^	lea "graph_start" rip rsi 
	^	lea "graph_end" rip rcx
	^	cpy 
	^	jmp_reg rax 
	*)
	^	"\n"
	^	"exit:\n" 
	^	fun_postfix
	^	"\n"

	^	error_exit
	^	"\n"
	^	"_end:\n"

