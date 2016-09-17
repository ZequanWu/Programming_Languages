fun match xs = (* [1,2,1,2,1,2] *)
  let fun s_need_one xs =
	case xs of
	    [] => true
	  | 1::xs' => s_need_two xs'
	  | _  => false
      and s_need_two xs =
	  case xs of
	      [] => true
	    | 2::xs' => s_need_one xs'
	    | _  => false
  in
      s_need_one xs
  end

datatype t1 = Foo of int | Bar of t2
     and t2 = Baz of string | Quux of t1

fun no_zeros_or_empty_strings_t1 x =
  case x of
      Foo i => i <> 0
    | Bar y => no_zeros_or_empty_strings_t2 y
and no_zeros_or_empty_strings_t2 x =
    case x of
	Baz s => size s > 0
      | Quux y => no_zeros_or_empty_strings_t1 y

(* code above works fine. This version works without any new language support *)
fun no_zeros_or_empty_strings_t1_alternate (f, x) =
  case x of
      Foo i => i <> 0
    | Bar y => f y

fun no_zeros_or_empty_strings_t2_alternate x =
  case x of
      Baz s => size s > 0
    | Quux y => no_zeros_or_empty_strings_t1_alternate (
		   no_zeros_or_empty_strings_t2_alternate, y)

structure MyModule =
struct

fun fact x =
  if x=0
  then 1
  else x* fact(x-1)

val half_pi = Math.pi / 2.0
end

signature MATHLIB =
sig
    val fact : int -> int
    val half_pi : real
end

structure MyMathLib :> MATHLIB =
struct
fun fact x = 10 * x
val half_pi = Math.pi / 2.0
fun doubler x = x * 2
end

signature RATIONAL_C =
sig
    type rational
    exception BadFrac
    val Whole : int -> rational
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end
					 
structure Rational3 :> RATIONAL_C =
struct
(* Invariant 1: all denominators > 0
   Invariant 2: rationals kept in reduced form, including that
                a Frac never has a denominator of 1 *)
  datatype rational = Whole of int | Frac of int*int
  exception BadFrac

  (* gcd and reduce hlep keep fractions reduced,
     but clients need not konw about them *)
  (* they _assume_ their inputs are not negative *)
  fun gcd (x, y) =
    if x = y
    then x
    else if x < y
    then gcd (x, y-x)
    else gcd (y, x)

  fun reduce r =
    case r of
	Whole _ => r
      | Frac (x, y) =>
	if x = 0
	then Whole 0
	else let val d = gcd (abs x, y) in (* using invariant 1 *)
		 if d = y
		 then Whole (x div d)
		 else Frac (x div d, y div d)
	     end

  (* when making as frac, we ban zero denominators *)
  fun make_frac (x, y) =
    if y = 0
    then raise BadFrac
    else if y < 0
    then reduce (Frac (~x, ~y))
    else reduce (Frac (x, y))

  (* using math properties, both invariants hold of the result
     assuming they hold of the arguments *)
  fun add (r1, r2) =
    case (r1, r2) of
	(Whole i, Whole j)         => Whole (i+j)
      | (Whole i, Frac (j, k))     => Frac (j+k*i, k)
      | (Frac (j, k), Whole i)     => Frac (j+k*i, k)
      | (Frac (a, b), Frac (c, d)) => reduce (Frac (a*d + b*c, b*d))

  (* given invariant, prints in reduced form *)
  fun toString r =
    case r of
	Whole i => Int.toString i
      | Frac (a, b) => (Int.toString a) ^ "/" ^ (Int.toString b)
						    
end

signature DIGIT = 
sig
    type digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
    val test : digit -> unit
end
    
structure Digit :> DIGIT =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end

signature COUNTER =
sig
    type t = int
    val newCounter : int -> t
    val increment : t -> t

end
structure NoNegativeCounter :> COUNTER = 
struct

exception InvariantViolated

type t = int

fun newCounter i = if i <= 0 then 1 else i

fun increment i = i + 1

fun first_larger (i1,i2) =
    if i1 <= 0 orelse i2 <= 0
    then raise InvariantViolated
    else (i1 - i2) > 0

end
