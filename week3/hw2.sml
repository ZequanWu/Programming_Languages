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
(* 1 (a) *)
fun all_except_option (str, sl) =
  let fun aux (prelist, restlist) =
	case restlist of
	    [] => NONE
	  | s::restlist' => if same_string (s, str)
			    then SOME (prelist @ restlist')
			    else aux (prelist @ [s], restlist')
  in aux ([], sl)
  end
      
(* 1 (b) *)
fun get_substitutions1 (sll, s) =
  case sll of
      [] => []
    | sl::sll' => case all_except_option (s, sl) of
		      NONE => [] @ get_substitutions1 (sll', s)
		    | SOME y => y @ get_substitutions1 (sll', s)

(* 1 (c) *)
fun get_substitutions2 (sll, s) =
  let fun aux (sll, acc) =
	case sll of
	    [] => acc
	  | sl::sll' => case all_except_option (s, sl) of
			    NONE => aux (sll', acc)
			  | SOME y => aux (sll', acc @ y)
  in
      aux(sll, [])
  end

(* 1 (d) *)
fun similar_names (sll, name) =
  let fun aux (sl, m, l) =
	case sl of
	    [] => []
	  | s::sl' => {first=s, middle=m, last=l} :: aux (sl', m, l)
  in case name of
	 {first=x,middle=y,last=z} =>
	 name :: aux (get_substitutions1 (sll, x), y, z)
  end

(* 2 (a) *)
fun card_color (s, _) =
  if s = Clubs orelse s = Spades then Black else Red

(* 2 (b) *)
fun card_value (_, r) =
  case r of
      Num i => i
    | Ace => 11
    | _  => 10

fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | c'::cs' => if c' = c
		 then cs'
		 else c'::remove_card(cs', c, e)

fun all_same_color cs =
  case cs of
      [] => true
    | _::[] => true
    | c1::(c2::c3) => card_color c1 = card_color c2
		      andalso all_same_color (c2::c3)

fun sum_cards cs =
  let fun aux (cs, acc) =
	case cs of
	    [] => acc
	  | c::cs' => aux (cs', acc + card_value c)
  in
      aux (cs, 0)
  end

fun score (cs, goal) =
  let val sum = sum_cards cs
      val pscore =  if sum > goal then 3*(sum-goal)
		    else goal-sum
  in
      if all_same_color cs
      then pscore div 2
      else pscore
  end

fun officiate (cs, ml, goal) =
  let
      fun aux (cs, hcs, ml) =
	case ml of
	    [] => score (hcs, goal)
	  | m::ml' => case (m, cs) of
			  (Draw, []) => score (hcs, goal)
			| (Draw, c::cs') => 
			  let val s = sum_cards (c::hcs)
			  in if s > goal then score (c::hcs, goal)
			     else aux (cs', c::hcs, ml')
			  end
			| (Discard c, _) =>
			  aux (cs, remove_card (hcs, c, IllegalMove), ml')
  in
      aux (cs, [], ml)
  end
