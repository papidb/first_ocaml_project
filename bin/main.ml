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

print_float new_float ;;

print_newline

(* lists *)

(* # [];; - : 'a list = []

   # [1; 2; 3];; - : int list = [1; 2; 3]

   # [false; false; true];; - : bool list = [false; false; true]

   # [[1; 2]; [3]; [4; 5; 6]];; - : int list list = [[1; 2]; [3]; [4; 5; 6]]

   I think one way of reading this type int list list

   is to read it from the back, so this is

   a list of list of intergers *)

let rec sum_of_list u =
  match u with
  | [] -> 0
  | x :: u -> x + sum_of_list u

let () = print_int (sum_of_list [1; 2; 4; 5; 2])
