exception Illegal_Index
val cbs : (int -> unit) list ref = ref []
fun onKeyEvent f = cbs := f::(!cbs) (* The only "public" binding *)
fun onEvent i =
  let fun loop fs =
	case fs of
	    [] => ()
	  | f::fs' => (f i; loop fs')
  in loop (!cbs)
  end

val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)

fun printIfPressed i =
  onKeyEvent (fn j => if i=j
		      then print ("you pressed " ^ Int.toString i ^ "\n")
		      else ()) 

val _ = printIfPressed 4
val _ = printIfPressed 11
val _ = printIfPressed 23

fun getNth l n =
  case l of
      [] => raise Illegal_Index
    | h::l' => if n = 0
	       then h
	       else getNth l' (n-1)	

datatype set = S of {insert : int -> set,
		     member : int -> bool,
		     size : unit -> int}

val empty_set =
    let
	fun make_set xs = (* xs is a "private field" in result *)
	  let (* contains a "private method" in result *)
	      fun contains i = List.exists (fn j => i=j) xs
	  in
	      S { insert = fn i => if contains i
				   then make_set xs
				   else make_set (i::xs),
		  member = contains,
		  size   = fn () => length xs
		}
	  end
    in
	make_set []
    end

fun use_sets () =
  let val S s1 = empty_set
      val S s2 = (#insert s1) 34
      val S s3 = (#insert s2) 34
      val S s4 = (#insert s3) 19 (* list : [19, 34] *)
  in
      if (#member s4) 42
      then 99	       
      else if (#member s4) 19
      then 17 + (#size s3) ()
      else 0
  end

datatype 'a mylist = Cons of 'a * ('a mylist) | Empty

fun map f xs =
  case xs of
      Empty => Empty
    | Cons(x,xs) => Cons(f x, map f xs)

fun filter f xs =
  case xs of
      Empty => Empty
    | Cons(x,xs) => if f x
		    then Cons(x,filter f xs)
		    else filter f xs

fun length xs =
  case xs of
      Empty => 0
    | Cons(_,xs) => 1 + length xs
