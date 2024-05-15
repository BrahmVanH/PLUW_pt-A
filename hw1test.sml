(* Tests for Homework 1 Functions *)

use "hw1VANHOUZEN.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1),(2014,2,1)],2) = 2

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 3) = "how"

val test7 = date_to_string (2024, 4, 20) = "April 20, 2024"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test95 = count(1, 5) = [1, 2, 3, 4, 5]

val test10 = month_range (30, 34) = [1, 1, 2, 2, 2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test12 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[3,3,3,3,3,3,3,3,3,3,2,3,4,4,4,4,4,2,2,2,2,4]) = 3

val test13 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4,4,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]


val test15 = reasonable_date (0, 2, 15) = false
val test15b = reasonable_date (2019, 0, 15) = false
val test15c = reasonable_date (2018, 2, 100) = false

val test15d = reasonable_date (1995, 2, 15) = true