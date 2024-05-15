(* Brahm Van Houzen
  Homework 1
  Programming Languages, Part A
  Coursera/University of Washington  *)

(* Takes in two dates, and returns a bool regarding whether the first date is before the second *)
fun is_older ((y1, m1, d1), (y2, m2, d2)) =
  y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
  

(* Takes in a data array, an (int * int * int) list and a month, and 
  returns a list containing all the dates from the list that are in the 
given month *)

fun dates_in_month(ds, m) = 
   case ds of 
    [] => []
    | (_: int, _: int, _: int)::_ =>
        let 
          fun get_m d = 
            let 
              val (_, m', _) = d
            in 
                (m' = m)
            end
        in
          List.filter get_m ds
        end

(*  This takes in an date list and a target month and returns 
  an int representing the amount of dates that fall in that month *)

fun number_in_month (ds, m) =
  case ds of 
    [] => 0
    | _::_ => length (dates_in_month(ds, m))
 

(* Takes in a date list and a list of months by number and returns 
  the amount of dates that are in any of the months*)

fun number_in_months (ds, ms) = 
  let 
    fun aux(ms', acc) =
      case ms' of 
        [] => acc
        | x::xs => aux(xs, acc + number_in_month(ds, x))
  in 
    aux(ms, 0)
  end


(* Takes in a date list and a month list and returns a list containing
  all the dates that fall in any of the months *)

fun dates_in_months (ds, ms) =
   let 
    fun aux(ms', acc) =
      case ms' of 
        [] => acc
        | x::xs => aux(xs, acc @ dates_in_month(ds, x))
  in 
    aux(ms, [])
  end


(* Takes in a list of strings and returns the nth *)

fun get_nth(ss, n) =
  case ss of 
    [] => ""
    | x::xs => 
        if 
          n = 1
            then x 
            else get_nth(xs, (n-1))


(* Takes in a date and returns a string representing that date *)

fun date_to_string (y, m, d) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    (get_nth(months, m))^" "^Int.toString d^", "^Int.toString y
  end



(* Takes in an array of sumbers and a sum and returns the position
  of the last element that, when added to its previous elements, gets
  the sum close enough to the given sum without going over  *)

fun number_before_reaching_sum(s, ns) =
      case ns of 
        [] => 0
        | x::xs => 
          if s <= x 
          then 0
          else 1 + number_before_reaching_sum (s - x, xs)


(* Takes in a day of the year (/365) and returns the 
  month the day occurs in by a month int *)

fun what_month d = 
let
  val ms = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
in 
  1 + number_before_reaching_sum(d, ms)
end


(* Takes in two ints and returns an array containing the two ints and
  all ints in between *)

fun count (a, b) =
	    if a=b
	    then b::[]
	    else a :: count(a+1, b)


(* Takes in two dates and returns a list containing the month number value
for each provided date and each date in between in ascending order *)

fun month_range (d1, d2) =
  if d1 > d2 
  then []
  else
      List.map what_month (count(d1, d2))


(* Takes in a list of dates and returns a date option
  representing the oldest date *)

fun oldest ds = 
  case ds of 
    [] => NONE
    | x::[] => SOME x 
    | y::ys => 
      let 
        fun oldest_of_two(d1, d2) =
          if is_older(d1, d2)
          then d1
          else d2
        fun helper max ds' =
          case ds' of 
            [] => SOME max
            | x::xs => helper(oldest_of_two(max, x)) xs
        in 
          helper y ys
        end

        


fun check(_, []) = false
  | check(x, y::xs) =
    x = y orelse check(x, xs)

(* This checks for and removes all duplicate occurences of elements
  returns as new list *)

fun remove_duplicates xs =
  case xs of 
    [] => []
    | y::ys => 
      let 
        val rest = remove_duplicates ys
      in 
        if check(y, rest)
            then rest
            else y::rest
      end
    

(* This behaves the same as number_in_months but have a month
  in the second argument multiple time has no effect different than
  a single time *)

fun number_in_months_challenge(ds, ms) =
    number_in_months(ds, remove_duplicates ms)

(* Behaves the same as dates_in_months but is not effected by repeats of
months as a second argument *)

fun dates_in_months_challenge (ds, ms) =
    dates_in_months(ds, remove_duplicates ms)



  (* This will take in a date and determine whether it is a 
  real date in the common era.  *)
fun reasonable_date d =
  let 
    fun get_nth(ns, n) =
      case ns of 
        [] => 0
        | x::xs => 
            if 
              n = 1
                then x 
                else get_nth(xs, (n-1))

    val (y, m, d') = d

     val leap = y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 <> 0)
    val feb_l = if leap then 29 else 28
    val lst = [31,feb_l,31,30,31,30,31,31,30,31,30,31]
 in 
    y > 0 andalso m >= 1 andalso m <= 12
    andalso d' >=1 andalso d' <= get_nth(lst, m)
  end
