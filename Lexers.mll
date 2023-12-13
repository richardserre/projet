{
  open Syntax
  open Lexing

(* useless since Ocaml 3.11 *)
(*
  let new_line lexbuf =
  let lcp = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { lcp with
    pos_lnum = lcp.pos_lnum + 1;
    pos_bol = lcp.pos_cnum;
  }
;;
*)
}
let digit = ['0'-'9']
let upper = ['A'-'Z']
let letter = ['a'-'z' 'A'-'Z']
let alphanum = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let printable = ['\032'-'\126' '\160'-'\255']
let string = '\"' (printable # ['\"'])+ '\"'
let char = '\'' (printable # ['\'']) '\''
rule token = parse
  | [' ' '\t']                { token lexbuf }
  | '\n'                      { new_line lexbuf; token lexbuf }
  | digit+ "." digit+ as num  { Float (float_of_string num) }
  | digit+ as num             { Integer (int_of_string num) }
  | char as txt               { Char (String.get txt 1) }
  | string as txt             { String (String.sub txt 1 ((String.length txt) - 2)) }
  | letter alphanum* as txt   { Id txt }
  | "\"\""     { String ("") }
  | "//"       { DBLSLASH }
  | "--#"      { DBLMINUSSHARP }
  | "(**"      { LPARDBLSTAR }
  | "*)"       { STARRPAR }
  | ";;"       { DBLSEMI }
  | "::"       { DBLCOLON }
  | "<="       { LESSEQ }
  | ">="       { GREATEREQ }
  | "<-"       { LEFTARROW }
  | "->"       { RIGHTARROW }
  | "=>"       { DBLRIGHTARROW }
  | "<>"       { LESSGREATER }
  | "!="       { NOTEQUALTO }
  | "/="       { DIFFER }
  | "=="       { EQUALTO }
  | "="        { EQUAL }
  | "@@"       { DBLAT }
  | ".."       { DBLDOT }
  | "||"       { DBLBAR }
  | "&&"       { DBLAMPERSAND }
  | ":="       { ASSIGN }
  | "--"       { DBLMINUS }
  | "--#"      { DBLMINUSSHARP }
  | "(*"       { comment1 lexbuf }
  | "/*"       { comment2 lexbuf }
  | "--"       { comment3 lexbuf }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { STAR }
  | '/'        { SLASH }
  | '^'        { CARET }
  | '('        { LPAR }
  | ')'        { RPAR }
  | ';'        { SEMI }
  | ','        { COMMA }
  | ':'        { COLON }
  | '/'        { SLASH }
  | '<'        { LESSTHAN }
  | '>'        { GREATERTHAN }
  | '.'        { DOT }
  | '?'        { QUESTION }
  | '{'        { LCURL }
  | '}'        { RCURL }
  | '['        { LSQUARE }
  | ']'        { RSQUARE }
  | '!'        { EXCLAM }
  | '#'        { SHARP }
  | '|'        { BAR }
  | '~'        { TILDE }
  | '_'        { UNDERSCORE }
  | '\\'       { BACKSLASH }
  | '`'        { BACKQUOTE }
  | '\''       { QUOTE }
  | '^'        { CARET }
  | '&'        { AMPERSAND }
  | '$'        { DOLLAR }
  | '%'        { PERCENT }
  | '@'        { AT }
  | _          { token lexbuf }
  | eof        { EOF }
and comment1 = parse
  | "*)"  { token lexbuf }
  | '\n'  { new_line lexbuf; comment1 lexbuf }
  | _     { comment1 lexbuf }
and comment2 = parse
  | "*/"  { token lexbuf }
  | '\n'  { new_line lexbuf; comment2 lexbuf }
  | _     { comment2 lexbuf }
and comment3 = parse
  | '\n'  { new_line lexbuf; token lexbuf }
  | _     { comment3 lexbuf }
