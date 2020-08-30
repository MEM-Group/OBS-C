
open Cabs


(**** translating expression ****)

let rec tranExp (e:expression) = match e with
      BINARY (ASSIGN, e1, e2) -> ((tranExp e1)^":="^(tranExp e2))
    | BINARY (ADD, e1, e2) -> ((tranExp e1) ^ "+" ^ (tranExp e2))
    | BINARY (SUB, e1, e2) -> ((tranExp e1) ^ "-" ^ (tranExp e2))
    | BINARY (MUL, e1, e2) -> ((tranExp e1) ^ "*" ^ (tranExp e2))
    | BINARY (DIV, e1, e2) -> ((tranExp e1) ^ "/" ^ (tranExp e2))
    | BINARY (MOD, e1, e2) -> ((tranExp e1) ^ "mod" ^ (tranExp e2))
    | LOCEXP (e,_) ->(match e with  
                        VARIABLE v -> v
                      | _ -> "")
    | CONSTANT (CONST_INT i) -> i
    | CONSTANT (CONST_FLOAT f) -> f
    | CONSTANT (CONST_STRING s) -> s
    | _ -> ""
      

(*************************************************)

let tranDecl nme = match nme with 
         (s,t,_,_) -> s

let rec tranTD ng = match ng with
    (_, nl) ->  match nl 
       with  [] -> ""
            | name :: rnames -> tranDecl name  

let tranStmt (s:statement) = match s with
     COMPUTATION (e,_) -> ((tranExp e) ^ ";\n")
    | _ -> "/**/"

let rec tranStmts (sl: statement list) = match sl with
      [] -> ""
    |s::rs -> (tranStmt s) ^ (tranStmts rs)

let tranBLK (b:block) = 
   match b with {blabels=sl; battrs = al; bstmts = bs} -> tranStmts bs

let transDef (dfn:definition) = 
  match dfn with 
    TYPEDEF (ng, _) -> tranTD ng
  | FUNDEF ((s,(n,_,_,_)),b,_,_) -> ("fn "^n^"{\n"^(tranBLK b)^"\n}\n")
  | _ -> ""               

let rec translation (ast: definition list) = 
   match ast with 
    [] -> ""
  | fdef :: rast -> (transDef fdef) ^ (translation rast)


(*************************************************)


