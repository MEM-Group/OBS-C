
open Cabs

let structSize: ((string * int) list) ref = ref [];;

let rec structsize (s: (string * int) list) na = match s with 
      [] -> 0
    | (na,n) :: res -> n
    | (na1,n) :: res -> structsize res na

(**** translating expression ****)

let processFname (fname:expression) (args:expression list) = 
    match fname with
      LOCEXP (e,_) -> (match e with 
                        VARIABLE "malloc" -> "allocateInit"
                      | VARIABLE "sizeof" -> (match args with [arg] -> (
                                                 match arg with LOCEXP (e,_) -> 
                                                   (match e with VARIABLE v -> (string_of_int (structsize (!structSize) v)) | _ -> "0") | _ -> "0"
                                              )
                                              | _ -> "")
                      | _ -> "" )
    | _ -> "xx"
             
let rec tranExps (es:expression list) = match es with
      [] -> ""
    | [e] -> (tranExp e)
    | e::res -> (tranExp e) ^ ", " ^ (tranExps res)

and  tranExp (e:expression) = match e with
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
    | CALL (fname, args) -> ("callcl "^(processFname fname args)^"("^ (tranExps args)^")")
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

let preprocessStruct (s: spec_elem) = 
    match s with 
      SpecType t -> (match t with
                        Tstruct (sname, fields, _) -> [(sname,(match fields with None -> 0 | Some l -> (List.length l)))]
                      | _ -> [] 
                    )
    | _ -> []

let rec preprocessStructs (ss: spec_elem list) = 
    match ss with 
      [] -> []
    | se :: rss -> (preprocessStruct se) @ (preprocessStructs rss)

let rec preprocessDef (d:definition) = match d with 
    ONLYTYPEDEF (sp,_) -> preprocessStructs sp
   |_ -> []  

let rec preprocess (ast: definition list) = 
   match ast with 
    [] -> []
  | d :: ds -> (preprocessDef d) @ (preprocess ds)

let rec printLS ls = match ls with 
      [] -> print_newline ()
    | (s,t) :: rs -> print_string s; print_string ": " ;print_int t; print_newline (); printLS rs

let rec translation (ast: definition list) = 
  structSize = ref (preprocess ast); 
   match ast with 
    [] -> ""
  | fdef :: rast -> (transDef fdef) ^ (translation rast)




