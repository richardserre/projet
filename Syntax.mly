%{
open Lexing
open Error
open Ast

let locate n = convert_region (rhs_start_pos n, rhs_end_pos n)
let irrelevant (name, _, _) = (name = "")

let uppercase token =
  match token with
  | Id s -> if String.capitalize_ascii s = s then CapId s else Id (String.uppercase_ascii s)
  | _ -> token

let rec flatten ty =
  match ty with
  | Product(tyl,tyr) -> (flatten tyl) @ (flatten tyr)
  | _ -> [ty]

let make_kwd token =
  match uppercase token with
  | Id "OPEN"   -> OPEN
  | Id "TYPE"   -> TYPE
  | Id "FUN"    -> FUN
  | Id "LET"    -> LET
  | Id "REC"    -> REC
  | Id "IN"     -> IN
  | Id "MATCH"  -> MATCH
  | Id "WITH"   -> WITH
  | Id "TRUE"   -> TRUE
  | Id "FALSE"  -> FALSE
  | Id "NOT"    -> NOT
  | Id "OF"     -> OF
  | Id "IF"     -> IF
  | Id "THEN"   -> THEN
  | Id "ELSE"   -> ELSE
  | Id "AND"    -> AND
  | Id "FORALL" -> FORALL
  | Id "EXISTS" -> EXISTS
  | Id "INT"    -> INT
  | Id "BOOL"   -> BOOL
  | Id "CHAR"   -> CHAR
  | Id "STRING" -> STRING
  | Id "UNIT"   -> UNIT
  | Id "LIST"   -> LIST
  | Id "TRY"    -> TRY
  | Id "RAISE"  -> RAISE
  | Id "VAL"    -> VAL
  | CapId s     -> CapId s
  | _           -> token

%}

%token OPEN   /* Keyword */
%token TYPE   /* Keyword */
%token FUN    /* Keyword */
%token LET    /* Keyword */
%token REC    /* Keyword */
%token IN     /* Keyword */
%token MATCH  /* Keyword */
%token WITH   /* Keyword */
%token TRUE   /* Keyword */
%token FALSE  /* Keyword */
%token NOT    /* Keyword */
%token OF     /* Keyword */
%token IF     /* Keyword */
%token THEN   /* Keyword */
%token ELSE   /* Keyword */
%token AND    /* Keyword */
%token FORALL /* Keyword */
%token EXISTS /* Keyword */
%token INT    /* Keyword */
%token BOOL   /* Keyword */
%token CHAR   /* Keyword */
%token STRING /* Keyword */
%token UNIT   /* Keyword */
%token LIST   /* Keyword */
%token TRY    /* Keyword */
%token RAISE  /* Keyword */
%token VAL    /* Keyword */

/* Identifiers */
%token <string> Id
%token <string> CapId

/* Literals */
%token <int> Integer
%token <float> Float
%token <string> String
%token <char> Char

/* Symbols */
%token EOF NEWLINE DBLMINUS LPARDBLSTAR
%token LPARSTAR SLASHSTAR STARRPAR STARSLASH DBLSLASH
%token DBLMINUSSHARP LPARSTARSLASH SLASHSTARRPAR LPAR RPAR
%token SEMI COMMA COLON SLASH DBLCOLON DBLSEMI
%token LESSTHAN GREATERTHAN LESSEQ GREATEREQ
%token LEFTARROW RIGHTARROW DBLRIGHTARROW
%token LESSGREATER NOTEQUALTO DIFFER EQUALTO EQUAL
%token PLUS MINUS STAR DOT QUESTION
%token DBLDOT DBLBAR DBLAMPERSAND LCURL RCURL
%token LSQUARE RSQUARE EXCLAM APOSTROPHE
%token SHARP BAR TILDE UNDERSCORE BACKSLASH BACKQUOTE
%token QUOTE CARET AMPERSAND ASSIGN DOLLAR PERCENT AT DBLAT

%right RIGHTARROW
%right ASSIGN
%nonassoc COLON
%left EQUAL
%left LESSTHAN GREATERTHAN
%left LESSEQ GREATEREQ
%left PLUS MINUS
%left STAR SLASH

%start main

%type <Ast.prog> main

%%

main        : prog sep
                { $1 }
            | prog sep EOF
                { $1 }
            | error
                { Error.fail (locate 1) "syntax error." }
;
prog        : prog_
                { let region = locate 1 in
                  if irrelevant region then $1
                  else Ast.ProgRegion ($1, region) }
;
prog_       : OPEN CapId prog
                { $3 }
            | TYPE LPAR ident_list RPAR Id EQUAL datatype prog
                { Ast.DataTypeDecl ($5, $3, $7, $8) }
            | TYPE QUOTE Id Id EQUAL datatype prog
                { Ast.DataTypeDecl ($4, [$3], $6, $7) }
            | TYPE Id EQUAL datatype prog
                { Ast.DataTypeDecl ($2, [], $4, $5) }
            | TYPE LPAR ident_list RPAR Id EQUAL record prog
                { Ast.RecordTypeDecl ($5, $3, $7, $8) }
            | TYPE QUOTE Id Id EQUAL record prog
                { Ast.RecordTypeDecl ($4, [$3], $6, $7) }
            | TYPE Id EQUAL record prog
                { Ast.RecordTypeDecl ($2, [], $4, $5) }
            | VAL ident COLON typ spec
                { Ast.Spec (($2, $4) :: $5) }
            | exp
                { Ast.Body ($1) }
;
ident       : UNDERSCORE
                { "_" }
            | Id
                { $1 }
;
idents      : ident
                { [$1] }
            | ident COMMA idents
                { ($1::$3) }
;
ident_seq   : ident
                { [$1] }
            | ident ident_seq
                { ($1::$2) }
;
ident_list  : QUOTE Id
                { [$2] }
            | QUOTE Id COMMA ident_list
                { ($2::$4) }
;
pattern     : LPAR ident DBLCOLON ident RPAR
                { ($2, $4) }
            | ident DBLCOLON ident
                { ($1, $3) }
;
sep         :  /* empty */
                { () }
            | DBLSEMI
                { () }
            | IN
                { () }
;
exp         : exp_
                { let region = locate 1 in
                  if irrelevant region then $1
                  else Ast.ExpRegion ($1, region) }
;
exp_        : EOF
                { Ast.Empty }
            | exp DBLAMPERSAND exp
                { Ast.And ($1, $3) }
            | exp DBLBAR exp
                { Ast.Or ($1, $3) }
            | exp LESSTHAN exp
                { Ast.Less ($1, $3) }
            | exp GREATEREQ exp
                { Ast.Not (Ast.Less ($1, $3)) }
            | exp GREATERTHAN exp
                { Ast.Greater ($1, $3) }
            | exp LESSEQ exp
                { Ast.Not (Ast.Greater ($1, $3)) }
            | exp EQUAL exp
                { Ast.Equal ($1, $3) }
            | exp PLUS exp
                { Ast.Plus ($1, $3) }
            | exp MINUS exp
                { Ast.Minus ($1, $3) }
            | exp STAR exp
                { Ast.Times ($1, $3) }
            | exp SLASH exp
                { Ast.Slash ($1, $3) }
            | exp AT exp
                { Ast.Append ($1, $3) }
            | exp CARET exp
                { Ast.Concat ($1, $3) }
            | NOT exp
                { Ast.Not $2 }
            | CapId exp_
                { match $2 with
                  | Pair (x,y) -> Ast.DataCons ($1, [x;y])
                  | _ -> Ast.DataCons ($1, [$2]) }
            | CapId LPAR exps RPAR
                { Ast.DataCons ($1, $3) }
            | exp DBLCOLON exp
                { Ast.Cons ($1, $3) }
            | IF exp THEN exp ELSE exp
                { Ast.IfThenElse ($2, $4, $6) }
            | LET ident EQUAL exp sep exp
                { Ast.Let ($2, $4, $6) }
            | AND Id EQUAL exp sep exp
                { Ast.Let ($2, $4, $6) }
            | letrec Id COLON typ EQUAL exp sep exp
                { Ast.Let' ($2, $4, $6, $8) }
            | AND Id COLON typ EQUAL exp sep exp
                { Ast.Let' ($2, $4, $6, $8) }
            | letrec Id params EQUAL exp sep exp
                { Ast.LetRec ($2, $3, Any, $5, $7) }
            | letrec Id params COLON typ EQUAL exp sep exp
                { Ast.LetRec ($2, $3, $5, $7, $9) }
            | letrec Id LPAR RPAR COLON typ EQUAL exp sep exp
                { Ast.LetRec ($2, [], $6, $8, $10) }
            | letrec Id LPAR RPAR EQUAL exp sep exp
                { Ast.LetRec ($2, [], Any, $6, $8) }
            | LET Id LPAR RPAR EQUAL exp sep exp
                { Ast.LetRec ($2, [], Any, $6, $8) }
            | LET Id COLON typ EQUAL exp sep exp
                { Ast.Let' ($2, $4, $6, $8) }
            | LET Id params EQUAL exp sep exp
                { Ast.LetRec ($2, $3, Any, $5, $7) }
            | LET Id params COLON typ EQUAL exp sep exp
                { Ast.LetRec ($2, $3, $5, $7, $9) }
            | LET Id LPAR RPAR COLON typ EQUAL exp sep exp
                { Ast.LetRec ($2, [], $6, $8, $10) }
            | LET LPAR ident COMMA ident RPAR EQUAL exp IN exp
                { Ast.Match ($8, $3, $5, $10) }
            | MATCH exp WITH LPAR ident COMMA ident RPAR RIGHTARROW exp
                { Ast.Match ($2, $5, $7, $10) }
            | MATCH exp WITH BAR LSQUARE RSQUARE RIGHTARROW exp BAR pattern RIGHTARROW exp
                { Ast.MatchList ($2, $8, $10, $12) }
            | MATCH exp WITH cases
                { Ast.MatchData ($2, $4) }
            | MATCH exp WITH LCURL record_pat RCURL RIGHTARROW exp
                { Ast.MatchRecord ($2, $5, $8) }
            | app_exp
                { match (List.rev $1) with
                  | [e] -> e
                  | ((Ast.Var f) :: [Ast.Empty]) -> Ast.Call (f, [])
                  | ((Ast.Var f) :: l) -> Ast.Call (f, l)
                  | ((Ast.Field (m, f)) :: [Ast.Empty]) -> Ast.Call' (m, f, [])
                  | ((Ast.Field (m, f)) :: l) -> Ast.Call' (m, f, l)
                  | _ -> Error.fail (locate 1) "syntax error." }
            | FUN params RIGHTARROW exp
                { Ast.Abs ($2, $4) }
            | TRY exp WITH CapId RIGHTARROW exp
                { Ast.TryWith ($2, $4, $6) }
            | RAISE CapId
                { Ast.Raise ($2) }
            | LET LPAR RPAR EQUAL exp_
                { Let("_", $5, Ast.Empty) }
            | LPAR exp RPAR
                { $2 }
;
simple_exp  : LPAR RPAR
                { Ast.Empty }
            | Id
                { Ast.Var $1 }
            | CapId
                { Ast.DataCons ($1, []) }
            | Id DOT Id
                { Ast.Field ($1, $3) }
            | CapId DOT Id
                { Ast.Field ($1, $3) }
            | Integer
                { Ast.IntCst $1 }
            | String
                { Ast.StringCst $1 }
            | Char
                { Ast.CharCst $1 }
            | TRUE
                { Ast.BoolTrue }
            | FALSE
                { Ast.BoolFalse }
            | LPAR exp COMMA exp RPAR
                { Ast.Pair ($2, $4) }
            | LSQUARE RSQUARE
                { Ast.Nil }
            | LSQUARE exp expsemi RSQUARE
                { Ast.ExplicitList ($2::$3) }
            | LCURL field_vals RCURL
                { ExplicitRecord $2 }
            | LCURL exp WITH field_vals RCURL
                { RecordWith($2, $4) }
            | LPAR exp RPAR
                { $2 }
;
app_exp:
            | simple_exp
                { [$1] }
            | app_exp simple_exp
                { ($2 :: $1) }
;
spec        : /* empty */
                { [] }
            | VAL ident COLON typ spec
                { (($2, $4) :: $5) }
;
cases       : /* empty */
                { [] }
            | BAR UNDERSCORE RIGHTARROW exp cases
                { ("_", [], $4)::$5 }
            | BAR CapId ident RIGHTARROW exp cases
                { ($2, [$3], $5)::$6 }
            | BAR CapId RIGHTARROW exp cases
                { ($2, [], $4)::$5 }
            | BAR CapId LPAR idents RPAR RIGHTARROW exp cases
                { ($2, $4, $7)::$8 }
;
optbar      : /* empty */
                { () }
            | BAR
                { () }
;
datatype    : /* empty */
                { [] }
            | optbar CapId datatype
                { ($2, [])::$3 }
            | optbar CapId OF typ datatype
                { ($2, (flatten $4))::$5 }
;
explist     : exp
                { [$1] }
            | exp explist
                { ($1::$2) }
;
letrec      : LET REC
                { () }
;
exps        : exp
                { [$1] }
            | exp COMMA exps
                { ($1::$3) }
;

expsemi     : /* empty */
                { [] }
            | SEMI
                { [] }
            | SEMI exp expsemi
                { ($2::$3) }
;
param       : LPAR ident COLON typ RPAR
                { ($2, $4) }
            | ident
                { ($1, Any) }
;
params      : param
                { [$1] }
            | param params
                { ($1::$2) }
;
record      : LCURL field_list RCURL
                { $2 }
;
field_list  : Id COLON typ
                { [($1, $3)] }
            | Id COLON typ SEMI
                { [($1, $3)] }
            | Id COLON typ SEMI field_list
                { (($1, $3)::$5) }
;
field_vals  : Id EQUAL exp
                { [($1, $3)] }
            | Id EQUAL exp SEMI field_vals
                { (($1, $3)::$5) }
;
record_pat  : Id EQUAL Id
                { [($1, $3)] }
            | Id EQUAL Id SEMI record_pat
                { (($1, $3)::$5) }
;
typ         : INT
                { Ast.Int }
            | BOOL
                { Ast.Bool }
            | CHAR
                { Ast.Char }
            | STRING
                { Ast.String }
            | UNIT
                { Ast.Unit }
            | QUOTE Id
                { Ast.Param $2 }
            | Id
                { Ast.Name ($1, []) }
            | typ Id
                { Ast.Name ($2, [$1]) }
            | LPAR typ_list RPAR Id
                { Ast.Name ($4, $2) }
            | typ LIST
                { Ast.List $1 }
            | typ RIGHTARROW typ
                { match $3 with
                  | Ast.Fun (l, r) -> Ast.Fun (($1::l), r)
                  | _ -> Ast.Fun ([$1], $3) }
            | typ STAR typ
                { Ast.Product ($1, $3) }
            | LPAR typ RPAR
                { $2 }
;
typs        : typ
                { [$1] }
            | typ STAR typs
                { ($1::$3) }
;
typ_list    : typ
                { [$1] }
            | typ COMMA typ_list
                { ($1::$3) }
;

%%

let main tokenizer = main (fun x -> make_kwd (tokenizer x))
