fun is_older((y1, m1, d1), (y2, m2, d2)) =
	y1 < y2 orelse (y1 = y2 andalso m1 < m2) 
	orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2);

(* can be implemented by reduce~? *)
fun number_in_month((date::nil):(int * int * int) list, month:int) = 
	if #2 date = month then 1 else 0
|	number_in_month((date::dates):(int * int * int) list, month:int) = 
	(if #2 date = month then 1 else 0) + number_in_month(dates, month);

fun number_in_months(dates:(int * int * int) list, (month::nil):int list) =
	number_in_month(dates, month)
|	number_in_months(dates:(int * int * int) list, (month::months):int list) =
	number_in_month(dates, month) + number_in_months(dates, months);

fun dates_in_month((date::nil):(int * int * int) list, month:int) = 
	if #2 date = month then (date::nil) else nil
|	dates_in_month((date::dates):(int * int * int) list, month:int) = 
	if #2 date = month then (date::dates_in_month(dates, month)) else dates_in_month(dates, month);

fun dates_in_months(dates:(int * int * int) list, (month::nil):int list) =
	dates_in_month(dates, month)
|	dates_in_months(dates:(int * int * int) list, (month::months):int list) =
	dates_in_month(dates, month)@dates_in_months(dates, months);

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

fun what_month days:int =
	let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
	in number_before_reaching_sum(days - 1, days_in_months)
	end;

fun month_range (day1:int, day2:int) =
	if day1 > day2 then []
	else what_month(day1)::month_range(day1 + 1, day2);

(* not sure: name conflict~? *)
fun oldest (date::dates:(int * int * int) list) =
	if null (date::dates) then NONE
	else 
		let
			fun oldest_helper(date::nil:(int * int * int) list, oldestDate:int * int * int) =
				if is_older(date, oldestDate) then oldestDate
				else date
			|	oldest_helper(date::dates:(int * int * int) list, oldestDate:int * int * int) =
				(* can merge something *)
				if is_older(date, oldestDate) then oldest_helper (dates, oldestDate)
				else oldest_helper (dates, date);
		in
			SOME (oldest_helper (dates, date))
		end;

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