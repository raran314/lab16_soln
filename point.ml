(*
                             CS51 Lab 16
                     Object-Oriented Programming
                        Two-Dimensional Points
*)

type point = float * float ;;

let add (x1, y1) (x2, y2) =
  x1 +. x2, y1 +. y2 ;;

let scale s (x, y) =
  s *. x, s *. y ;;
  
let offset (x, y) distance angle =
  x +. distance *. cos angle,
  y +. distance *. sin angle ;;

