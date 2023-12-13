(* This file is auto-generated, do not modify *)

open Syntax

(* idents are case-sensitive *)
let uppercase token =
  match token with
  | Id s ->
      if String.contains s '.' then Id s (* long_id *)
      else
        let c = String.get s 0 in
        if c >= 'A' && c <= 'Z' then CapId s else Id (String.uppercase_ascii s)
  | _ -> token

let make token =
  match uppercase token with
  | Id "OPEN" -> OPEN
  | Id "TYPE" -> TYPE
  | Id "FUN" -> FUN
  | Id "LET" -> LET
  | Id "REC" -> REC
  | Id "IN" -> IN
  | Id "MATCH" -> MATCH
  | Id "WITH" -> WITH
  | Id "TRUE" -> TRUE
  | Id "FALSE" -> FALSE
  | Id "NOT" -> NOT
  | Id "OF" -> OF
  | Id "IF" -> IF
  | Id "THEN" -> THEN
  | Id "ELSE" -> ELSE
  | Id "AND" -> AND
  | Id "FORALL" -> FORALL
  | Id "EXISTS" -> EXISTS
  | Id "INT" -> INT
  | Id "BOOL" -> BOOL
  | Id "CHAR" -> CHAR
  | Id "STRING" -> STRING
  | Id "UNIT" -> UNIT
  | Id "LIST" -> LIST
  | Id "TRY" -> TRY
  | Id "RAISE" -> RAISE
  | Id "VAL" -> VAL
  | CapId s -> CapId s
  | _ -> token
