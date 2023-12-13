type ident = string
type typ =
  | Any
  | Unit
  | Int
  | Bool
  | Char
  | String
  | Param of ident (* Type variable, example: 'a *)
  | Name of ident * typ list (* User defined type, example: (int, bool) tree *)
  | List of typ (* Primitive List type, example: int list *)
  | Fun of typ list * typ (* n-ary function type *)
  | Product of typ * typ (* binary product type *)

type exp =
  | Var of ident
  | Empty
  | IntCst of int
  | StringCst of string
  | CharCst of char
  | BoolTrue
  | BoolFalse
  | Plus of exp * exp
  | Times of exp * exp
  | Slash of exp * exp
  | Minus of exp * exp
  | Equal of exp * exp
  | Less of exp * exp
  | Greater of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Call of ident * exp list
  | Call' of ident * ident * exp list
  | Abs of (ident * typ) list * comm
  | Pair of exp * exp
  | Nil
  | Cons of exp * exp
  | ExplicitList of exp list
  | Append of exp * exp
  | Concat of exp * exp
  | DataCons of ident * exp list
  | ExplicitRecord of (ident * exp) list
  | RecordWith of exp * (ident * exp) list
  | Field of ident * ident
  (* commands *)
  | IfThenElse of exp * comm * comm
  | Let of ident * exp * comm
  | Let' of ident * typ * exp * comm
  | LetRec of ident * (ident * typ) list * typ * comm * comm
  | Match of exp * ident * ident * comm
  | MatchList of exp * exp * (ident * ident) * comm
  | MatchData of exp * (ident * (ident list) * comm) list
  | MatchRecord of exp * (ident * ident) list * comm
  | Raise of ident
  | TryWith of exp * ident * comm
  | ExpRegion of exp * Error.region
and comm = exp

type prog =
  | RecordTypeDecl of ident * ident list * (ident * typ) list * prog
  | DataTypeDecl of ident * ident list * (ident * typ list) list * prog
  | Body of exp
  | Spec of (ident * typ) list
  | ProgRegion of prog * Error.region
