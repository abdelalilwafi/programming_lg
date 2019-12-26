(* dates : (int * int * int) list *)


fun is_older(xs : int * int * int, ys : int * int * int) = 
  if (#1 xs)=(#1 ys)
  then if (#2 xs)=(#2 ys)
        then (#3 xs) < (#3 ys)
        else (#2 xs) < (#2 ys)
  else (#1 xs) < (#1 ys)




(* function number_in_month that takes a list of dates and a month (i.e., an in and returns how many dates in the list are in the given month.*)

(* input :  int list of date ,  int represent month *)

(* output -> int *)

fun number_in_month(xs : (int * int * int) list, y : int) = 
  if null xs
  then 0
  else 
    let val step = number_in_month(tl xs, y)
    in
      if (#2 (hd xs)) = y
      then 1 + step
      else step
    end


(* function 3 *)
(* input : list of dates, list of month *)
(* output is a int *)
fun number_in_months(xs : (int*int*int) list, ys : int list) = 
        if null ys
        then 0
        else
          number_in_month(xs, hd ys) + number_in_months(xs, tl ys);



(* function 4 *)
(* input :  a list of dates, a int month
* output  -> a list of date that contient this month *)

fun dates_in_month(xs : (int * int * int) list , y : int ) =
  if null xs
  then []
  else
    let val st = dates_in_month(tl xs, y)
    in
      if (#2 (hd xs)) = y
      then (hd xs) :: st
      else st
    end

(* function 5 *)
(* input : list of dates,  list of months
*output -> list of dates that contient those month *)
fun dates_in_months(dates : (int * int * int) list, months : int list ) = 
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* function 6 *)
(* input : string, int -> string *)
fun get_nth(xs : string list, n : int) =
  if null xs
  then ""
  else if n = 1
        then hd xs
        else get_nth(tl xs, n - 1);




(* function n° 7 *)

(* input : date -> string *)
fun date_to_string(date : int * int * int ) =
   let val month = get_nth(["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November","December"], #2 date);
     val year = Int.toString(#1 date);
     val day = Int.toString(#3 date);
        in
         month ^ " " ^ day ^", " ^ year
        end

(* function n° 8 *)
(* input : int , int list -> int *)
fun number_before_reaching_sum(sum : int, nums : int list) = 
    if (sum - (hd nums))<=0 orelse null (tl nums)
    then hd nums
    else number_before_reaching_sum(sum - (hd nums), tl nums);



(* function n° 9 *)
(* input : int -> int *)
fun what_month(n : int) = 
    let
      fun number_before_reaching_sums(sum : int, nums : (int * int) list) =
        if (sum - ((#2 (hd nums))))<=0 orelse null (tl nums)
        then (#1 (hd nums))
        else number_before_reaching_sums(sum - (#2 (hd nums)), tl nums);
    in
    number_before_reaching_sums(n,
    [(1, 31),(2, 29),(3, 31),(4, 30),(5, 31),(6, 30),(7, 31),(8, 30),(9,
    31),(10,30),(11, 31),(12, 30)])
    end




    

(* function n° 11 *)
fun oldest(xs : (int * int * int) list) = 
  if null xs
  then NONE
  else
    if null (tl xs)
    then SOME (hd xs)
    else
      let val st = oldest(tl xs)
      in
        if is_older(valOf st, hd xs)
        then SOME (hd xs)
        else
          st
      end
