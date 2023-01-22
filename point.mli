(*
                             CS51 Lab 16
                     Object-Oriented Programming
                        Two-Dimensional Points
*)

(* A point type in x-y coordinates *)
type point = float * float ;;

(* add point1 point2 -- Returns the vector sum of the two
   points *)
val add : point -> point -> point ;;

(* scale factor point -- Returns the point scaled by the factor *)
val scale : float -> point -> point ;;

(* offset point distance angle -- Returns the point reached by going
   the distance at an angle (in radians) *)
val offset : point -> float -> float -> point ;;
