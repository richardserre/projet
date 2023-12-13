
type ident = string
type typ =
  | Any
  | Unit
  | Int
  | Bool
  | Char
  | String
  | Param of ident                (* Type variable, example: 'a *)
  | Name of ident * typ list      (* User defined type, example: (int, bool) tree *)
  | List of typ                  (* Primitive List type, example: int list *)
  | Fun of typ list * typ              (* n-ary function type *)
  | Product of typ * typ (* binary product type *)


let rec string_of_typ (t : typ) =
   match t with
 
  | Any -> failwith "'Any' unexpected"
  | Unit -> "unit"
  | Int -> "int"
  | Bool -> "booleen"
  | Char -> "char"
  | String -> "string"
  | Param (st) -> "'"^st
  | Name (st,list_of_t) -> "("^parcour_list (list_of_t)^") " ^st       
  | List (t) -> string_of_typ(t)^" list"
  | Fun (list_of_t,t) -> "("^parcour_list_fun (list_of_t)^") : "^string_of_typ(t)^" fun"
  | Product (t1,t2) -> "("^string_of_typ(t1)^", "^string_of_typ(t2)^")"
  | _ -> failwith "Unsupported feature in typ"
and 
  parcour_list (t: typ list)=
	match t with
	| [] -> ""
	| [h] -> " "^string_of_typ (h)
	| h::t -> ""^string_of_typ (h)^", "^parcour_list(t)
and
  parcour_list_fun (t: typ list)=
	match t with
	| [] -> ""
	| [h] -> " "^string_of_typ (h)
	| h::t -> " "^string_of_typ (h)^" -> "^parcour_list_fun(t)


let t1 : typ = Param ("a") ;;
print_endline (string_of_typ t1) ;;
let t2: typ = Name("tree",[Int;Int;Int;Bool;Param("a")]);;
print_endline (string_of_typ t2) ;;
let t3: typ = List(Int);;
print_endline (string_of_typ t3) ;;
let t4: typ=List(Param("b"));;
print_endline (string_of_typ t4) ;;
let t5: typ = Fun ([Int;Bool;Name("tree",[Param("a");Param("b")])],Int  );;
print_endline (string_of_typ t5) ;;
let t6: typ = Product(Int,Bool);;
print_endline (string_of_typ t6) ;;
let t7: typ = Product(Product(Int,Int),Int);;
print_endline (string_of_typ t7) ;;


