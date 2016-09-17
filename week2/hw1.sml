fun is_older(date1 : int*int*int, date2 : int*int*int) =
  if #1 date1 > #1 date2
  then false
  else if #1 date1 < #1 date2
  then true
  else if #2 date1 > #2 date2
  then false
  else if #2 date1 < #2 date2
  then true
  else if #3 date1 > #3 date2
  then false
  else #3 date1 < #3 date2

fun number_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else if #2 (hd dates) =  month
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)
	   
fun number_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then hd dates :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
							 
fun get_nth(lostr : string list, n : int) =
  if n = 1
  then hd lostr
  else get_nth(tl lostr, n-1)

fun date_to_string(date : int*int*int) =
  let
      val months = ["January", "February", "March", "April", "May", "June",
		    "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, lon : int list) =
  let
      val temp = hd lon
  in
      if temp >= sum
      then 0
      else if temp + hd (tl lon) >= sum
      then 1
      else 1 + number_before_reaching_sum(sum, (temp + hd (tl lon)) :: (tl (tl lon)))
  end

fun what_month(doy : int) =
  let
      val months = [31,28,31,30,31,30,31,31,30,31,30,31];
  in
      number_before_reaching_sum(doy, months)+1
  end
      
fun month_range(doy1 : int, doy2 : int) =
  if doy1 > doy2
  then []
  else what_month(doy1) :: month_range(doy1+1, doy2)
	   
fun oldest(lod : (int*int*int) list) =
  if null lod
  then NONE
  else if null (tl lod)
  then SOME (hd lod)
  else let val old = oldest(tl lod)
       in if isSome old andalso is_older(hd lod, valOf old)
	  then SOME (hd lod)
	  else old
       end
