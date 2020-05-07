let rip = "%rip"
let eax = "%eax"
let rax = "%rax"
let rbx = "%rbx"
let rcx = "%rcx"
let rdx = "%rdx"
let rsi = "%rsi" 
let rdi = "%rdi"

let rsp = "%rsp"
let r15 = "%r15"
let r14 = "%r14"
let r13 = "%r13" 

let nop = 
		"\t\tnop\n"

let add src tar  =
		"\t\tadd\t\t"^src^", "^tar^"\n"

let sub src tar  =
		"\t\tsub\t\t"^src^", "^tar^"\n"

let div reg =
		"\t\tdiv\t\t"^reg^"\n"

let mov src tar  =
		"\t\tmov\t\t"^src^", "^tar^"\n"

let movabs src tar  =
		"\t\tmovabs\t\t"^src^", "^tar^"\n"

let movb src tar =
		"\t\tmovb\t\t"^src^", "^tar^"\n"

let movw src tar =
		"\t\tmovw\t\t"^src^", "^tar^"\n"

let movd src tar =
		"\t\tmovd\t\t"^src^", "^tar^"\n"

let movq src tar =
		"\t\tmovq\t\t"^src^", "^tar^"\n"

let lea label base reg  =
		"\t\tlea\t\t"^label^"("^base^"), "^reg^"\n" 

let jmp_reg reg  =
		"\t\tjmp\t\t*"^reg^"\n"

let jmp label  =
		"\t\tjmp\t\t"^label^"\n" 

let jne label  =
		"\t\tjne\t\t"^label^"\n" 

let je label  =
		"\t\tje\t\t"^label^"\n"

let cmp im reg  =
		"\t\tcmp\t\t"^im^", "^reg^"\n"

let cmpb im reg  =
		"\t\tcmpb\t\t"^im^", "^reg^"\n"

let rep_movsb =
		"\t\trep\t\tmovsb\n"

let bswap reg = 
		"\t\tbswap\t\t"^reg^"\n"

let mem offset reg =
		string_of_int offset ^ "("^reg^")"

let push data = 
		"\t\tpush\t\t"^data^"\n"

let pop reg =
		"\t\tpop\t\t"^reg^"\n"

let lookup offset reg =
		movq (string_of_int offset^"("^rsp^")") reg
