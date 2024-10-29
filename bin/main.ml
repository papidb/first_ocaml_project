(* prints a ints in a list *)
let print_list lst =
  (* Print a newline at the beginning *)
  print_newline () ;
  (* Define a helper function for the recursive printing *)
  let rec aux = function
    | [] -> ()
    | e :: l -> print_int e ; print_string " " ; aux l
  in
  (* Call the helper function *)
  aux lst ;
  (* Print a newline at the end *)
  print_newline ()

let () = print_endline "\nHello, World!"

let () = print_int 10

let () = print_endline ""

(* here is a comment in oCaml*)
(* here is another comment in oCaml (* and this is a nested comment *)*)
let () = print_int (50 (* a comment in between an expression *) + 2)

let _u = [1; 2; 3; 4; 5]

let result = 2 * if "hello" = "world" then 1 else 5

let () = print_int result

(* feets in a mile *)
let feets = 5280

let x = 50

let y = x * feets ;;

(* _c is unbound so you can't print it or do anything with it it's an
   expression, we say define a name _c then bind it to 50, it is then used in
   the expression to get 2500 *)
let _c = 50 in
y * y
;;

(* This defines two names: a with value 1 and b with value 2. Then the
   example uses them in the expression a + b, resulting in the value of 3. *)
let a = 1 in
let b = 2 in
a + b

let square x = x * x

let square_of_10 = square 10

let cat a b = a ^ "" ^ b

let cat_hi = cat "\nhi"

let laugh = cat "ha" "ha"

let hi_daniel = cat (cat_hi "daniel") "\n"

let () = print_endline hi_daniel

let () = print_int square_of_10

let () = print_endline laugh

let omo = (fun x : int -> x * x) 50

let () = print_int omo

(* Type parameters and Higher order function *)
let array_hof = [1; 2; 3; 4]

let _squared_array_hof = List.map square array_hof ;;

(* let print_list list = List.iter (printf "%d ") list;; *)
print_list _squared_array_hof

(* print_list ["hi"; "daniel"];; *)

let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi

let one_to_ten = range 1 10 ;;

print_list one_to_ten

(* addition + is for intergers +. is for floats *)

(* type conversion *)

let new_float = float_of_int 1 +. 2.45 ;;

print_float new_float

(* lists *)

(* # [];; - : 'a list = []

   # [1; 2; 3];; - : int list = [1; 2; 3]

   # [false; false; true];; - : bool list = [false; false; true]

   # [[1; 2]; [3]; [4; 5; 6]];; - : int list list = [[1; 2]; [3]; [4; 5; 6]]

   I think one way of reading this type int list list

   is to read it from the back, so this is

   a list of list of intergers *)

let poly_list = [1; 2; 4; 5; 2]

let rec sum_of_list u =
  match u with
  | [] -> 0
  | x :: u -> x + sum_of_list u

let () = print_int (sum_of_list poly_list) ;;

print_newline ()

(*
length_of_list is a polymorphic function
this more or less means it operates on any kind of list
a list of ints, strings, etc. 

Why? 
the match [] has no type, so it can be of any type
the match _ :: b we don't inspect the type at the head of the list
Since both patterns must be of the same type, 
the typing algorithm infers the 'a list -> int type
*)
let rec length_of_list u =
  match u with
  | [] -> 0
  | _ :: b -> 1 + length_of_list b

let () = print_int (length_of_list poly_list) ;;

print_newline ()

let rec map_list f u =
  match u with
  | [] -> []
  | x :: u -> f x :: map_list f u
;;

print_newline () ;;

map_list (fun x -> print_int x) poly_list ;;

print_newline () ;;

print_list (map_list square poly_list)

(*
   fun i ->
      match i with
      | 0 -> 1
      | _ -> 0
*)

let snd p =
  match p with
  | _, y -> y
;;

(*
   The type of tuples is written using * between the components' types.
   The type of snd is 'a * 'b -> 'b
*)
print_endline (snd (42, "apple"))

(*
   Pattern Matching
   Pattern Matching in OCaml
   Pattern matching in OCaml is much more versatile than switch statements.
   Rather than just matching simple values, OCaml's pattern matching can decompose complex data structures (like lists, tuples, option types, and custom types) directly.
   It allows for conditions that depend on the shape or structure of the data, not just its value.
*)

let describe_list lst =
  match lst with
  | [] -> "The list is empty."
  | [x] -> "The list has one element: " ^ string_of_int x
  | [x; y] ->
      "The list has two elements: " ^ string_of_int x ^ " and "
      ^ string_of_int y
  | _ :: _ :: _ -> "The list has more than two elements."
;;

print_endline (describe_list [1]) ;;

print_endline (describe_list [1; 2]) ;;

print_endline (describe_list [1; 2; 3]) ;;

print_endline (describe_list [1; 2; 3; 4])

(* variant types *)
(* Like pattern matching generalises switch statements, variant types generalise enumerated and union types. *)

(* primary_color is an example of an enumerated type (fancy for enum) *)
type primary_color = Red | Green | Blue

let primary_color_list = [Red; Blue; Green] ;;

map_list
  (fun color ->
    match color with
    | Red -> print_string "red" ; print_string " "
    | Green -> print_string "green" ; print_string " "
    | Blue -> print_string "blue" ; print_string " " )
  primary_color_list
