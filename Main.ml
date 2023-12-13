open Parsers
open Printer
open Pretty

(* Build this project with "esy" and "dune": 
   esy dune build Main.bc

   Execute the code with "esy" and "dune": 
   esy dune exec -- ./Main.bc
*)

(* Run the parser interactively in the Ocaml Toplevel as follows.
   Run the ocaml command and then type:

   #directory "_build/default/.Main.eobjs/byte/";;
   #load_rec "parsers.cmo";;
   Parsers.parse_from_file "Examples/concat.ml";;

*)

let usage command = 
  print_endline ("Command: " ^ command);
  print_endline ("Usage: Main.bc [-pretty] source.ml");
  print_endline ("   converts 'source.ml' from mini-ML syntax to TypeScript syntax");
  print_endline ("   use option '-pretty' to generate a mini-ML source file"); 
  exit 0

let translate pretty filename =
  let p = parse_with_regions filename in
  let modulename = Filename.chop_extension (Filename.basename filename) in
  begin
    if pretty then
      print_string (Pretty.string_of_full_prog modulename p)
    else
      print_string (Printer.string_of_full_prog modulename p)
  end

let check ml_file1 ml_file2 =
  let p1 = parse_with_regions ml_file1 in
  let p2 = parse_with_regions ml_file2 in
  let modulename = Filename.chop_extension (Filename.basename ml_file1) in
  let s1 = Pretty.string_of_full_prog modulename p1 in
  let s2 = Pretty.string_of_full_prog modulename p2 in
  begin
    if s1 = s2 then ()
    else print_string "Warning: pretty-printed programs are differents.\n"
  end

let check_AST ml_file1 ml_file2 =
  (* parse without regions *)
  let p1 = parse_from_file ml_file1 in
  let p2 = parse_from_file ml_file2 in
  begin
    if p1 = p2 then ()
    else print_string "Warning: AST are differents.\n"
  end

let () =
  let argv = Array.to_list Sys.argv in
  let command = List.hd argv in
  match argv with
  | (_ :: "-check" :: ml_file1 :: ml_file2 :: []) -> check ml_file1 ml_file2 
  | (_ :: "-pretty" :: ml_file :: []) -> translate true ml_file
  | (_ :: ml_file :: []) -> translate false ml_file
  | _ -> usage command
