open Syntax
open Lexing

let main tokenizer =
  Syntax.main (fun x -> Keywords.make (tokenizer x))

let parse_from_string s = main Lexers.token (Lexing.from_string s)
let parse_from_channel cin = main Lexers.token (Lexing.from_channel cin)
let parse_from_file name = parse_from_channel (open_in name)

let parse_with_regions name =
  let lexbuf = from_channel (open_in name) in
  let pos = lexbuf.lex_curr_p in
  begin
    lexbuf.lex_curr_p <- { pos with pos_fname = name };
    main Lexers.token lexbuf
  end
