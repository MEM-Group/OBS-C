
open Cabs


exception Undefine of string

let rec structsize (s: (string * int) list) na = match s with 
      [] -> 0
    | (na,n) :: res -> n
    | (na1,n) :: res -> structsize res na

let rec print_structsize (l: (string * int) list) = match l with
      [] -> print_newline ()
    | (s,n) :: res -> print_string s; print_string ": "; print_int n; print_newline (); print_structsize res

let tranFname (fname:expression) = 
    match fname with
      LOCEXP (e,_) ->  (match e with 
                          VARIABLE "malloc" -> "allocateInit"
                        |  _ -> "" )
    | _ -> ""

let tranInitName (itn:init_name) = match itn with
      (name,ie) -> match name with (n,_,_,_) -> ("let " ^ n ^ ";") 

let rec tranInitNames (itns:init_name list) = match itns with
      [] -> ""
    | fi :: ritns -> (tranInitName fi) ^ (tranInitNames ritns)

let rec tranDfn dfn = match dfn with
    (* (statement) -> block -> block_element_list -> declaration ->
       decl_spec_list ->  *)
      DECDEF (ing,_) -> match ing with 
                        (_,il) -> tranInitNames il
    | _ -> raise (Undefine "tranDfn")
                 
             
let rec tranExps (es:expression list) sinfo = match es with
      [] -> ""
    | [e] -> (tranExp e sinfo)
    | e::res -> (tranExp e sinfo) ^ ", " ^ (tranExps res sinfo)
and  tranExp (e:expression) sinfo = match e with
      BINARY (ASSIGN, e1, e2) -> ((tranExp e1 sinfo)^":="^(tranExp e2 sinfo))
    | BINARY (ADD, e1, e2) -> ((tranExp e1 sinfo) ^ "+" ^ (tranExp e2 sinfo))
    | BINARY (SUB, e1, e2) -> ((tranExp e1 sinfo) ^ "-" ^ (tranExp e2 sinfo))
    | BINARY (MUL, e1, e2) -> ((tranExp e1 sinfo) ^ "*" ^ (tranExp e2 sinfo))
    | BINARY (DIV, e1, e2) -> ((tranExp e1 sinfo) ^ "/" ^ (tranExp e2 sinfo))
    | BINARY (MOD, e1, e2) -> ((tranExp e1 sinfo) ^ "mod" ^ (tranExp e2 sinfo))
    | LOCEXP (e,_) -> tranExp e sinfo
    | VARIABLE s -> s
    | CONSTANT (CONST_INT i) -> i
    | CONSTANT (CONST_FLOAT f) -> f
    | CONSTANT (CONST_STRING s) -> s
    | TYPE_SIZEOF (sp,dt) -> (
            match sp with 
              [se] -> (match se with 
                         SpecType ts -> (
                             match ts with 
			       Tnamed tn -> string_of_int (structsize sinfo tn)
                             | Tstruct (tn,_,_) -> string_of_int (structsize sinfo tn)
			     | _ -> "" )
                       | _ -> "" )
	   )
    | CALL (fname, args) -> ("callcl "^(tranFname fname)^"("^ (tranExps args sinfo)^")")
    | _ ->  ""
      

(*************************************************)

let tranDecl nme = match nme with 
         (s,t,_,_) -> s

let rec tranTD ng = match ng with
    (_, nl) ->  match nl 
       with  [] -> ""
            | name :: rnames -> tranDecl name  

let tranStmt (s:statement) sinfo = match s with
      COMPUTATION (e,_) -> ((tranExp e sinfo) ^ ";\n")
    | DEFINITION dfn -> tranDfn dfn
    | _ -> raise (Undefine "tranStmt")

let rec tranStmts (sl: statement list) sinfo = match sl with
      [] -> ""
    |s::rs -> (tranStmt s sinfo) ^ (tranStmts rs sinfo)

let tranBLK (b:block) sinfo = 
   match b with {blabels=sl; battrs = al; bstmts = bs} -> tranStmts bs sinfo

let transDef (dfn:definition) sinfo = 
  match dfn with 
    TYPEDEF (ng, _) -> tranTD ng
  | FUNDEF ((s,(n,_,_,_)),b,_,_) -> ("fn "^n^"{\n"^(tranBLK b sinfo )^"\n}\n")
  | _ -> ""              

  let preprocessStruct (s: spec_elem) =
    match s with 
      SpecType t ->  (match t with
		       Tstruct (sname, fields, _) -> [(sname,
						      (match fields with
							 None ->  0
						       | Some l -> (List.length l)))]
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


let rec trans (ast: definition list) sinfo = 
   match ast with 
    [] -> ""
  | fdef :: rast -> (transDef fdef sinfo) ^ (trans rast sinfo)

  let translation (ast: definition list) =
    let sinfo = preprocess ast in 
    trans ast sinfo



