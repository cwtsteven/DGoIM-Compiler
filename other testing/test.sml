
val d_1 = (fn f => fn x => f (x 0) (x 0)) ;

val d_2 = (fn f => fn x => f (d_1 f x) (d_1 f x)) ;

val d_3 = (fn f => fn x => f (d_2 f x) (d_2 f x)) ;

val d_4 = (fn f => fn x => f (d_3 f x) (d_3 f x)) ;

val d_5 = (fn f => fn x => f (d_4 f x) (d_4 f x)) ;

val d_6 = (fn f => fn x => f (d_5 f x) (d_5 f x)) ;

val d_7 = (fn f => fn x => f (d_6 f x) (d_6 f x)) ;

val d_8 = (fn f => fn x => f (d_7 f x) (d_7 f x)) ;

val d_9 = (fn f => fn x => f (d_8 f x) (d_8 f x)) ;

val d_10 = (fn f => fn x => f (d_9 f x) (d_9 f x)) ;

val d_11 = (fn f => fn x => f (d_10 f x) (d_10 f x)) ;

val d_12 = (fn f => fn x => f (d_11 f x) (d_11 f x)) ;

val d_13 = (fn f => fn x => f (d_12 f x) (d_12 f x)) ;

val d_14 = (fn f => fn x => f (d_13 f x) (d_13 f x)) ;

val d_15 = (fn f => fn x => f (d_14 f x) (d_14 f x)) ;

val d_16 = (fn f => fn x => f (d_15 f x) (d_15 f x)) ;

val d_17 = (fn f => fn x => f (d_16 f x) (d_16 f x)) ;

val d_18 = (fn f => fn x => f (d_17 f x) (d_17 f x)) ;

val d_19 = (fn f => fn x => f (d_18 f x) (d_18 f x)) ;

val d_20 = (fn f => fn x => f (d_19 f x) (d_19 f x)) ;

val d_21 = (fn f => fn x => f (d_20 f x) (d_20 f x)) ;

val d_22 = (fn f => fn x => f (d_21 f x) (d_21 f x)) ;

val d_23 = (fn f => fn x => f (d_22 f x) (d_22 f x)) ;

val d_24 = (fn f => fn x => f (d_23 f x) (d_23 f x)) ;

val d_25 = (fn f => fn x => f (d_24 f x) (d_24 f x)) ;

val _ = d_20 (fn x => fn y => fn z => 0) (fn f => fn x => 0) ;