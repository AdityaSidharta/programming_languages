fun is_older(date1 : int * int * int, date2 : int * int * int) = 
    let val year1 = #1 date1;
        val year2 = #1 date2;
        val month1 = #2 date1;
        val month2 = #2 date2;
        val day1 = #3 date1;
        val day2 = #3 date2;
    in if year1 < year2
       then true
       else if year1 > year2
            then false
            else if month1 < month2
                 then true
                 else if month1 > month2
                      then false 
                      else if day1 < day2
                      then true
                      else false
    end;


fun number_in_month(dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else let val date = hd(dates);
             val remain_dates = tl(dates);
         in if #2 date = month
            then 1 + number_in_month(remain_dates, month)
            else 0 + number_in_month(remain_dates, month)
         end;

fun number_in_months(dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else let val month = hd(months);
             val remain_months = tl(months);
         in number_in_month(dates, month) + number_in_months(dates, remain_months)
         end;

fun dates_in_month(dates: (int * int * int) list, month: int) = 
    if null dates
    then []
    else let val date = hd(dates);
             val remain_dates = tl(dates);
         in if #2 date = month
            then date :: dates_in_month(remain_dates, month)
            else dates_in_month(remain_dates, month)
         end;

fun dates_in_months(dates: (int * int * int) list, months: int list) = 
    if null months
    then []
    else let val month = hd(months);
             val remain_months = tl(months);
         in dates_in_month(dates, month) @ dates_in_months(dates, remain_months)
         end;

fun get_nth(strings: string list, n: int) =
    if n < 1
    then raise Fail "n cannot be smaller than 1."
    else if n = 1
         then hd strings
         else get_nth(tl strings, n-1);

fun date_to_string(date: (int * int * int)) = 
    let val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
        val year = #1 date;
        val month = #2 date;
        val day = #3 date;
    in get_nth(month_names, month)^" "^Int.toString(day)^", "^Int.toString(year)
    end;

fun number_before_reaching_sum(sum:int, numbers:int list) = 
    if null numbers
    then raise Fail "sum is bigger than the summation of all the numbers given"
    else let val number = hd(numbers);
             val remain_numbers = tl(numbers);
             val remain_sum = sum - number;
         in if remain_sum <= 0
            then 0
            else 1 + number_before_reaching_sum(remain_sum, remain_numbers)
         end;

fun what_month(day_of_year: int) = 
    let val month_days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 1 + number_before_reaching_sum(day_of_year, month_days)
    end;

fun month_range(day_of_year1: int, day_of_year2: int) = 
    let val month1 = what_month(day_of_year1);
    in if day_of_year1 > day_of_year2
       then []
       else month1 :: month_range(day_of_year1 + 1, day_of_year2)
    end;

fun oldest(dates: (int*int*int) list) =
    if null dates
    then NONE
    else if null(tl(dates))
         then SOME(hd(dates))
         else let val first_date = hd(dates);
                  val second_date = hd(tl(dates));
                  val remain_dates = tl(tl(dates));
              in if is_older(first_date, second_date)
                 then oldest(first_date :: remain_dates)
                 else oldest(second_date :: remain_dates)
              end;