fun is_older(xs : int * int * int, ys : int * int * int) = 
  if (#1 xs)=(#1 ys)
  then if (#2 xs)=(#2 ys)
        then (#3 xs) < (#3 ys)
        else (#2 xs) < (#2 ys)
  else (#1 xs) < (#1 ys)




(* function number_in_month that takes a list of dates and a month (i.e., an in and returns how many dates in the list are in the given month.*)

(* input -> int list of date  &&  -> int represent month *)

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




fun number_in_months(xs : (int*int*int) list, ys : int list) = 
        if null ys
        then 0
        else
          number_in_month(xs, hd ys) + number_in_months(xs, tl ys);
