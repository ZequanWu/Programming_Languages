exception ListLengMismatch

val r = {foo = 2,bar = "asdf", idk = true}

datatype mytype = TwoInts of int*int
		| Str of string
		| Pizza

(* mytype -> int *)
(* case mathing must exhaust all cases and not redundant cases *)
fun f (x : mytype) =
  case x of
      Pizza => 3
    | Str s => String.size s
    | TwoInts(i1, i2) => i1+i2

datatype id = StudentNum of int
	    | Name of string
		      * (string option)
		      * string

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp

(* Add (Constant (10+9), Negate (Constant (5))) *)

fun eval e =
  case e of
      Constant i        => i
    | Negate e2         => ~ (eval e2)
    | Add (e1, e2)      => (eval e1) + (eval e2)
    | Multiply (e1, e2) => (eval e1) * (eval e2)

fun eval2 Constant i = i
  | eval2 Negate e = ~ (eval e)
  | eval2 Add (e1, e2) = (eval2 e1) + (eval2 e2)
  | eval2 Multiply (e1, e2) = (eval2 e1) * (eval2 e2)
					       
fun number_of_adds e = (* exp -> int *)
  case e of
      Constant i        => 0
    | Negate e2         => number_of_adds e2
    | Add (e1, e2)      => 1 + number_of_adds e1 + number_of_adds e2
    | Multiply (e1, e2) => number_of_adds e1 + number_of_adds e2
							      
fun max_constant e =
  case e of
      Constant i        => i
    | Negate e2         => max_constant e2
    | Add (e1, e2)      => Int.max(max_constant e1, max_constant e2)
    | Multiply (e1, e2) => Int.max(max_constant e1, max_constant e2)

datatype my_int_list = Empty
		     | Cons of int * my_int_list

fun append_my_list (xs,ys) =
  case xs of
      Empty => ys
    | Cons (x, xs') => Cons (x, append_my_list (xs', ys))

fun int_or_zero intoption =
  case intoption of
      NONE => 0
    | SOME i => i+1

fun sum_list xs =
  case xs of
      [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
      [] => ys
    | x::xs' => x::append (xs', ys)

fun append2 ([], ys) = ys
  | append2 (x::xs', ys) = x :: append2(xs', ys)
				       
datatype 'a option  = NONE | SOME of 'a
datatype 'a mylist = Empty | Cons of 'a * 'a mylist
datatype ('a, 'b) tree = Leaf of 'b | Node of 'a * ('a, 'b) tree * ('a, 'b) tree

(* type is (int, int) tree -> int *)
fun sum_tree tr =
  case tr of
      Leaf i => i
    | Node (i, left, right) => i + sum_tree left + sum_tree right

(* type is ('a int) tree -> 'a *)
fun sum_leaves tr = 					
  case tr of
      Leaf i => i
    | Node (i, left, right) => sum_leaves left + sum_leaves right

(* type is ('a, 'b) tree -> int *)
fun num_leaves tr =
  case tr of
      Leaf i => 1
   | Node (i, left, right) => num_leaves left + num_leaves right

(* fun f p = e *)
fun sum_triple (x, y, z) =
  x + y + z

fun full_name {first = x, middle = y, last = z} =
  x ^ " " ^ y ^ " " ^ z

(* ''a * ''a -> string *)
fun same_thing (x, y) =
  if x = y then "yes" else "no"

(* int -> string *)
fun is_three x =
  if x = 3 then "yes" else "no"

(* a' list * a' list * a' list -> a'*a'*a' list *)
fun zip list_tuple =
  case list_tuple of
      ([], [], []) => []
    | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip (tl1, tl2, tl3)
    | _  => raise ListLengMismatch

(* a'*a'*a' list -> a' list * a' list * a' list *)
fun unzip lst =
  case lst of
      [] => ([], [], [])
    | (a, b, c)::tl => let val (l1, l2, l3) = unzip tl
		       in
			   (a::l1, b::l2, c::l3)
		       end

fun nondecreasing xs =
  case xs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => head <= neck andalso
			    nondecreasing (neck::rest)

datatype sign = P | N | Z
			    
(* int*int -> sign *)
fun multsign (x1, x2) =
  let fun sign x = if x = 0 then Z else if x > 0 then P else N
  in
      case (sign x1, sign x2) of
	  (_, Z) => Z
	| (Z, _) => Z
	| (P, P) => P
	| (N, N) => P
	| _  => N
  end
(* a' list -> int *)
fun len xs =
  case xs of
      [] => 0
    | _::xs' => 1 + len xs'

(* tail recursion *)
(* a' list -> a' list *)
fun rev xs =
  let fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x :: xs' => aux (xs', x :: acc)
  in
      aux (xs, [])
  end
