(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun all_except_option (str : string, sl : string list) =
  let fun f (prelist : string list, restlist : string list) =
	case restlist of
	    [] => NONE
	  | s::restlist' => if same_string (s, str)
			    then SOME (prelist @ restlist')
			    else f (prelist @ [s], restlist')
  in f ([], sl)
  end

fun get_substitutions1 (sll : string list list, s : string) =
  case sll of
      [] => []
    | sl::sll' => case all_except_option (s, sl) of
		      NONE => []
		    | SOME y => y @ get_substitutions1 (sll', s)
