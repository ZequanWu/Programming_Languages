(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

(* fn : (unit -> int) -> (string -> int) -> pattern -> int *)
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* (1) *)
val  only_capitals =
  List.filter (fn s => (Char.isUpper o String.sub) (s, 0))

(* (2) *)
val longest_string1 =
  foldl (fn (s1, s2) =>
	    if String.size s1 > String.size s2
	    then s1 else s2) ""

(* (3) *)
val longest_string2 =
  foldl (fn (s1, s2) =>
	    if String.size s1 >= String.size s2
	    then s1 else s2) ""
	
(* (4) *)
fun longest_string_helper f =
  if f (1,1) then longest_string2 else longest_string1

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x <= y)
  
(* (5) *)
val longest_capitalized = longest_string1 o only_capitals

(* (6) *)
val rev_string = String.implode o rev o String.explode
  
(* (7) *)
fun first_answer f alist =
  case alist of
      [] => raise NoAnswer
    | a::alist => case f a of
		      NONE => first_answer f alist
		    | SOME y => y

(* (8) *)
fun all_answers f alist =
  let fun aux list_option rest_list =
	case rest_list of
	    [] => list_option
	  | a::rest_list => case (f a, list_option) of
				(SOME l, SOME l') => aux (SOME (l' @ l)) rest_list
			      | _  => NONE
  in aux (SOME []) alist
  end

(* (9) *)
(* a *)
val count_wildcards =
  g (fn () => 1) (fn _ => 0)
    
(* b *)
val count_wild_and_variable_lengths =
    g (fn () => 1) String.size
      
(* c *)
fun count_some_var (s, p) =
  g (fn () => 0) (fn s1 => if s1 = s then 1 else 0) p

(* (10) *)
fun check_pat p =
  let fun form_sl p =
	case p of
	    Variable s => [s]
	  | TupleP ps => List.foldl (fn (p,l) => form_sl p @ l) [] ps
	  | ConstructorP (_, p)  => form_sl p
	  | _  => []
      fun check_str sl =
	case sl of
	    [] => false
	  | s::sl => List.exists (fn x => x = s) sl orelse check_str sl
  in
      (not o check_str o form_sl) p 
  end

(* (11) *)
fun match (v, p) =
  case (v, p) of
      (_, Wildcard) => SOME []
    | (_, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP j) => if i = j then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
			       then all_answers match (ListPair.zip (vs, ps))
			       else NONE
    | (Constructor (s1, v), ConstructorP (s2, p)) =>
      if s1 = s2 then match (v,p) else NONE
    | _ => NONE
			   
(* (12) *)
fun first_match v ps =
  SOME (first_answer (fn x => match (v, x)) ps)
  handle NoAnswer => NONE
