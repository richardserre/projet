open Lexing

type region = string * (int * int) * (int * int)

let convert pos = (pos.pos_lnum, 1 + (pos.pos_cnum - pos.pos_bol))

let convert_region (start_pos, end_pos) =
  (start_pos.pos_fname, convert start_pos, convert end_pos)

let string_of_position (l, c) = string_of_int l ^ "." ^ string_of_int c

let string_of_region (name, p1, p2) =
  name ^ ":" ^ string_of_position p1 ^ "-" ^ string_of_position p2

let fail (reg : region) (msg : string) =
  prerr_string (string_of_region reg ^ (": " ^ msg) ^ "\n");
  exit 1
