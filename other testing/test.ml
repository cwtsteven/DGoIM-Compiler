
let d_1 = (fun f -> fun x -> f (x 0) (x 0)) in

let d_2 = (fun f -> fun x -> f (d_1 f x) (d_1 f x)) in

let d_3 = (fun f -> fun x -> f (d_2 f x) (d_2 f x)) in

let d_4 = (fun f -> fun x -> f (d_3 f x) (d_3 f x)) in

let d_5 = (fun f -> fun x -> f (d_4 f x) (d_4 f x)) in

let d_6 = (fun f -> fun x -> f (d_5 f x) (d_5 f x)) in

let d_7 = (fun f -> fun x -> f (d_6 f x) (d_6 f x)) in

let d_8 = (fun f -> fun x -> f (d_7 f x) (d_7 f x)) in

let d_9 = (fun f -> fun x -> f (d_8 f x) (d_8 f x)) in

let d_10 = (fun f -> fun x -> f (d_9 f x) (d_9 f x)) in

let d_11 = (fun f -> fun x -> f (d_10 f x) (d_10 f x)) in

let d_12 = (fun f -> fun x -> f (d_11 f x) (d_11 f x)) in

let d_13 = (fun f -> fun x -> f (d_12 f x) (d_12 f x)) in

let d_14 = (fun f -> fun x -> f (d_13 f x) (d_13 f x)) in

let d_15 = (fun f -> fun x -> f (d_14 f x) (d_14 f x)) in

let d_16 = (fun f -> fun x -> f (d_15 f x) (d_15 f x)) in

let d_17 = (fun f -> fun x -> f (d_16 f x) (d_16 f x)) in

let d_18 = (fun f -> fun x -> f (d_17 f x) (d_17 f x)) in

let d_19 = (fun f -> fun x -> f (d_18 f x) (d_18 f x)) in

let d_20 = (fun f -> fun x -> f (d_19 f x) (d_19 f x)) in

let d_21 = (fun f -> fun x -> f (d_20 f x) (d_20 f x)) in

let d_22 = (fun f -> fun x -> f (d_21 f x) (d_21 f x)) in

let d_23 = (fun f -> fun x -> f (d_22 f x) (d_22 f x)) in

let d_24 = (fun f -> fun x -> f (d_23 f x) (d_23 f x)) in

let d_25 = (fun f -> fun x -> f (d_24 f x) (d_24 f x)) in

d_25 (fun x -> fun y -> 0) (fun x -> 0)