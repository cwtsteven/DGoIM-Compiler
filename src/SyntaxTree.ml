type var = string

type binary_op = Add 
			   | Sub
			   | Mul
			   | Div
			   | Mod
			   | Eq
			   | Neq
			   | Gt
			   | Geq
			   | Lt
			   | Leq
			   | And 
			   | Or 

type term = Var of var 
		  | Const of int
		  | Abs of var * term
		  | App of term * term
		  | Add of term * term

(* printing the tree, so tedious *)

(*
let string_of_binary_op op = match op with
	| Add 	 ->  "Add"
	| Sub 	 ->  "Sub"
	| Mul 	 ->  "Mul"
	| Div 	 ->  "Div"
	| Mod  	 ->  "Mod"
	| Eq 	 ->  "Eq"
	| Neq	 ->  "Neq"
	| Gt	 ->  "Gt"
	| Geq	 ->  "Geq"
	| Lt 	 ->  "Lt"
	| Leq	 ->  "Leq"
	| And 	 ->  "And"
	| Or 	 ->  "Or"
*)

let rec string_of_term (term : term) = 
	match term with
	| Var v -> v
	| Const n -> string_of_int n
	| Abs (v, t) -> "\\" ^ v ^ "." ^ string_of_term t
	| App (t, u) -> "(" ^ string_of_term t ^ ") (" ^ string_of_term u ^ ")"
	| Add (t, u) -> "(" ^ string_of_term t ^ ") + (" ^ string_of_term u ^ ")"
