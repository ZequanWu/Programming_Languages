fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)
		       
fun list_product(l : int list) =
  if null l
  then 1
  else (hd l) * list_product(tl l)

fun countdown(x : int) =
  if x = 0
  then []
  else x :: countdown(x-1)

fun append(xs : int  list, ys : int list) =
  if null xs
  then ys
  else (hd xs) :: (append ((tl xs), ys))

fun sum_pair_list (xs : (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)
					      
fun firsts(xs : (int * int) list) =
  if null xs
  then []
  else (#1 (hd xs)) :: firsts(tl xs)

fun seconds (xs : (int * int) list) =
  if null xs
  then []
  else (#2 (hd xs)) :: seconds(tl xs)

fun sum_pair_list2(xs : (int * int) list) =
  (sum_list (firsts xs)) + (sum_list (seconds xs))
			       
fun factorial(n : int) =
  if n = 0
  then 1
  else n*factorial(n-1)

fun factorial2(n : int) =
  list_product(countdown(n))

fun silly1 (z : int) =
  let
      val x = if z > 0 then z else 34
      val y = x + z + 9
  in
      if x > y then x * 2 else y * y
  end
      
fun silly2() =
  let
      val x = 1
  in
      (let val x = 2 in x + 1 end) + (let val y = x+2 in y+1 end)
  end

fun countup_from1(x : int) =
  let
      fun count(from : int) =
	if from = x
	then x ::[]
	else from :: count(from+1)
  in
      count(1)
  end

fun good_max(xs : int list) =
  if null xs
  then 0
  else if null (tl xs)
  then hd xs
  else
      let val tl_ans = good_max(tl xs)
      in
	  if hd xs > tl_ans
	  then hd xs
	  else tl_ans
      end

fun max1 (xs : int list) =
  if null xs
  then NONE
  else
      let val tl_ans = max1(tl xs)
      in if isSome tl_ans andalso valOf tl_ans > hd xs
	 then tl_ans
	 else SOME(hd xs)
      end

fun max2 (xs : int list) =
  if null xs
  then NONE
  else let
      fun max_nonempty(xs : int list) =
	if null (tl xs)
	then hd xs
	else let val tl_max = max_nonempty(tl xs)
	     in
		 if hd xs > tl_max
		 then hd xs
		 else tl_max
	     end
  in
      SOME (max_nonempty xs)
  end
	   

