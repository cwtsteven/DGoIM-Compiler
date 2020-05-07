let n_var = 300


let _ = 

	print_string ("let f0 = (fun x -> x) in\n"); 
	for sym = 1 to n_var do 
		print_string("let f"^string_of_int sym^" = (fun x -> f"^string_of_int (sym-1)^" (f"^string_of_int (sym-1)^" x)) in\n");
	done;
	print_string("f"^string_of_int n_var^" f0\n")