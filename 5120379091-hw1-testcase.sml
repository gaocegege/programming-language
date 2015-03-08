val date1 = (1987,1,1);
val date2 = (1987,2,2);
val date3 = (1987,1,1);
val date4 = (1987,1,1);
val date5 = (1987,1,1);
val date6 = (2010,1,1);
val date7 = (1987,1,10);
val date8 = (1987,10,1);
val date9 = (1987,10,1);
val date10 = (1987,10,10);

val list_of_date1 = [ date1,date2,date3,date4,date5,date6,date7,date8,date9,date10];

val list_of_month = [1,10];
val list_of_month1 = [2,3];
val list_of_month2 = [3];

val list_of_string1 = ["1","2","3","4","5","6","7","8","9","10"];

is_older(date1, date2);
is_older(date3, date4);
is_older(date5, date6);
is_older(date7, date8);
is_older(date9, date10);
is_older(date2, date1);
is_older(date4, date3);
is_older(date6, date5);
is_older(date8, date7);
is_older(date10, date9);

number_in_month(list_of_date1,1);
number_in_month(list_of_date1,10);
number_in_month(list_of_date1,2);
number_in_months(list_of_date1,list_of_month);
number_in_months(list_of_date1,list_of_month1);
number_in_months(list_of_date1,list_of_month2);

dates_in_month(list_of_date1,1);
dates_in_month(list_of_date1,10);
dates_in_month(list_of_date1,2);

dates_in_months(list_of_date1,list_of_month);
dates_in_months(list_of_date1,list_of_month1);
dates_in_months(list_of_date1,list_of_month2);

get_nth(list_of_string1,1);
get_nth(list_of_string1,5);
get_nth(list_of_string1,11);
get_nth(list_of_string1,0);
get_nth(list_of_string1,~1);

date_to_string(date1);
date_to_string(date2);
date_to_string(date10);

number_before_reaching_sum(55,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]);
number_before_reaching_sum(101,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]);


what_month(365);
what_month(31);
what_month(50);

month_range(31,32);
month_range(30,33);

oldest(list_of_date1);

number_in_months_challenge(list_of_date1, list_of_month);
dates_in_months_challenge(list_of_date1, list_of_month);