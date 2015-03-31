fun is_older((y1, m1, d1), (y2, m2, d2)) =
	y1 < y2 orelse (y1 = y2 andalso m1 < m2) 
	orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2);

fun number_in_month(days: (int*int*int) list, month: int) =
	if null days then 0
	else number_in_month(tl days, month)
		+ (if #2 (hd days) = month then 1 else 0);

fun number_in_months(days: (int*int*int) list, months: int list) =
	if null months then 0
	else number_in_month(days, hd months)+number_in_months(days, tl months);

fun dates_in_month(days:(int*int*int) list, month: int) =
	if null days then []
	else (if #2 (hd days) = month then hd days :: dates_in_month(tl days, month) else dates_in_month(tl days, month));

fun dates_in_months(days: (int*int*int) list, months: int list) =
	if null months then []
	else dates_in_month(days, hd months) @ dates_in_months(days, tl months);
fun get_nth(str::nil: string list, n: int) = 
	if n > 1 then ""
	else str
|	get_nth(str::strs: string list, n: int) = 
	if n = 1 then str 
	else get_nth(strs, n-1);

fun date_to_string(date : int * int * int) = 
	let val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

fun number_before_reaching_sum (sum:int, (ele::lst):int list) =
	if sum > ele then 1 + number_before_reaching_sum(sum - ele, lst)
	else 0;

fun what_month(n: int) =
	number_before_reaching_sum(n, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])+1;

fun month_range(day1: int, day2: int) =
	if day1 > day2 then []
	else what_month(day1) :: month_range(day1+1, day2);

(* not sure: name conflict~? *)
fun oldest(days: (int*int*int) list) =
	if null days then NONE
	else 
		let
			val tl_ans = oldest(tl days)
		in
			if isSome tl_ans andalso is_older(valOf tl_ans, hd days)
			then tl_ans			
			else SOME (hd days)
		end

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
	let 
		fun contains(values:int list, value:int) =
			if null values
			then false
			else 
				if value = (hd values)
				then true
				else contains(tl values,value)

		fun remove_duplicate(m:int list,z: int list) = 
			if null m
			then []
			else 
				if contains(z,hd m) 
				then remove_duplicate(tl m,z)
				else (hd m) :: remove_duplicate(tl m,hd m::z)
	in
		number_in_months(dates,remove_duplicate(months,[]))
	end

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
	let 
		fun contains(values:int list, value:int) =
			if null values
			then false
			else 
				if value = (hd values)
				then true
				else contains(tl values,value)

		fun remove_duplicate(m:int list,z: int list) = 
			if null m
			then []
			else 
				if contains(z,hd m) 
				then remove_duplicate(tl m,z)
				else (hd m) :: remove_duplicate(tl m,hd m::z)
	in
		dates_in_months(dates,remove_duplicate(months,[]))
	end

fun reasonable_date(date: int*int*int) =
    let 
        val year = #1 date
        val month = #2 date
        val day = #3 date

        fun is_leap_year() =
           if (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)
           then true
           else false

        fun is_day_valid(days_in_months:int list,month_index:int) =
           if month_index - 1 = 0
           then
                 day <= hd days_in_months
           else 
               is_day_valid(tl days_in_months, month_index - 1)

    in    
     
     if year >=1 andalso month >= 1 andalso month <= 12 andalso
                         day >= 1 andalso day <=366

     then
          if is_leap_year()
          then
               is_day_valid([31,29,31,30,31,30,31,31,30,31,30,31], month)
          else
               is_day_valid([31,28,31,30,31,30,31,31,30,31,30,31], month)
     
     else false

    end