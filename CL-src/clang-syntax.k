
/*!

\title{The Syntax of Core Languages for Rust}

*/

/*@
\section{Part 2. The Syntax of Items}
*/

require "memory-syntax.k"
require "importcl.k"
require "clveri.k"


module CLANG-SYNTAX

imports MEMORY-SYNTAX
imports IMPORTCL
imports DOMAINS
imports DOMAINS-SYNTAX
imports CLVERI

syntax Rust ::= ""

syntax Rust
       ::= ExpCL			//This is for testing

syntax ExpCL
       ::= "if" ExpCL "then" ExpCL "else" ExpCL		[klabel(ifexpcl)]
	 | "let" Ident "=" ExpCL		[klabel(#letcl), strict(2)]
	 | ExpCL ";" ExpCL					[right,klabel(seqexpcl)]

syntax Id
       ::= "main"



syntax ExpCL
       ::= Ident
         | ErrorState
         | ValueCL
	 | #cvarcl(ExpCL)			[strict]
	 | MemOp
	 | "*" ExpCL			[klabel(dereferencecl)]
	 | ExpCL "." ExpCL			[klabel(fieldcl)]
	 | refcl(Int,ExpCL)			[klabel(referencecl)]
	 | #refallocate(ExpCL)			[strict]
	 | callcl(ExpCL, ExpCLs)		[klabel(funapplycl)]
	 | pcallcl(ExpCL, ExpCL ,ExpCLs)	[klabel(pfunapplycl)]
	 | ArithExpCL	   		
	 | ExpCL ":=cl" ExpCL			[klabel(assignExpcl)]
	 | FnCL
	 | "(|" ExpCL "|)"			[klabel(blockcl)]
	 | ( ExpCL )				[bracket]
	 | "assertcl" "(" ExpCL ")"		[klabel(#assertcl), strict]
	 | "clskip"
	 | #move(ExpCL)
	 | #moveVal(ExpCL,ExpCL)		[strict(1)]
	 | StrExp
	 | #paniccl(ExpCL)			[strict]
	 | #rawcl(ExpCL)
	 | #print(String, ExpCLs)		[strict(2)]
	 | #retcl(ExpCL)			
	 | #endThread(Int)
	 | "#loop" ExpCL "{" ExpCL "}"		[klabel(#loopcl)]
	 | #loopstart(ExpCL)
	 | "#break"
	 | #drop(ExpCL)
	 | #wait(ExpCL)				[strict]
	 | #deallocate(ExpCL)
	 | "#destructThread"
	 //The following is undefined
         | #memcopy(ExpCL,ExpCL,ExpCL)		[strict(3)]
         | #offset(ExpCL, ExpCL)		[seqstrict]
         | #reallocate(ExpCL, ExpCL)		[seqstrict]
         | #AllocateInit(ExpCL,CoreTy,ExpCLs)   [seqstrict(1,3)]
         | #AllocateInitRaw(ExpCL,CoreTy,ExpCLs)   [seqstrict(1,3)]
         | #Allocate(ExpCL,CoreTy)		[strict(1)]
         | #RawCL(ExpCL)			[strict]
	 | #FieldA(ExpCL, ExpCL)		[strict(2)]
	 | #Index(ExpCL,ExpCL)
	 | "#stop"



syntax StrExp
       ::= #lenStrcl(ExpCL)			[strict]


syntax ExpCL
       ::= #forkcl(ExpCL, ExpCL)		[seqstrict]
	 | "case" ExpCL "of" "{" ExpCLs "}"	[strict(1),klabel(#casecl1)]
	 | "case" "{" GuardedActions  "}"	[klabel(#casecl2)]

syntax GuardedActions
       ::= List{GuardedAction, ";"}		[klabel(#guardedActions)]


syntax GuardedAction
       ::= ExpCL "->" ExpCL			[klabel(#guardedAction)]




syntax Ident
       ::= Id
         | "#anonymous"

syntax Ident
       ::= FunId

syntax FunId
       ::= funId(Int)
         | #funIdcl(Int)

syntax Id
       ::= #env(Int)
         | #envId(Int)




syntax priorities
         dereferencecl
       > fieldcl
       > funapplycl
       > arithmulcl arithdivcl arithpowcl 
       > arithpluscl arithminuscl
       > arithmodcl arithequcl lesseqcl lesscl greatcl greateqcl
       > assignExpcl
       > #letcl ifexpcl
       > seqexpcl
     



syntax ArithExpCL
       ::= left:
	   ExpCL "**" ExpCL				[left, strict(1,2),klabel(arithmulcl)]
	 | ExpCL "/" ExpCL				[left, strict(1,2),klabel(arithdivcl)]
	 | ExpCL "^" ExpCL				[left, strict(1,2),klabel(arithpowcl)]
	 > left:
	   ExpCL "+" ExpCL				[left, strict(1,2),klabel(arithpluscl)]
	 | ExpCL "-" ExpCL				[left, strict(1,2),klabel(arithminuscl)]
	 > left:
	   ExpCL "==" ExpCL				[left, strict(1,2),klabel(arithequcl)]
	 | ExpCL "<=" ExpCL				[left, strict(1,2),klabel(lesseqcl)]
	 | ExpCL "<" ExpCL				[left, strict(1,2),klabel(lesscl)]
	 | ExpCL ">" ExpCL				[left, strict(1,2),klabel(greatcl)]
	 | ExpCL ">=" ExpCL				[left, strict(1,2),klabel(greateqcl)]
	 | ExpCL "mod" ExpCL				[left, strict(1,2),klabel(arithmodcl)]
	 > left:
	   ExpCL "&&" ExpCL				[left, strict(1,2),klabel(#andcl)]
	 | ExpCL "||" ExpCL				[left, strict(1,2),klabel(#orcl)]
	 | ExpCL "&" ExpCL				[left, strict(1,2),label(#bitandcl)]

syntax Int
       ::= boolval(Bool)		[function]		


syntax Order
       ::= "sc"
         | "na"


syntax FnCL
       ::= "fun" Ident "(" Idents ")" "{" ExpCL "}"
         | "fun" Ident "[" Ident "]" "(" Idents ")" "{" ExpCL "}"
         | "fun" "(" Idents ")" "{" ExpCL "}"




syntax Idents
       ::= List{Ident, ","}			[klabel(identscl)]


syntax ExpCLs
       ::= List{ExpCL, ","}			[strict,klabel(#ExpCLs)]


syntax ExpCLs
       ::= ValueCLs


syntax Id
       ::= #cpath(TP,Ident)			
         | #tpathcl(ExpCL,Ident)
	 | #tmppath(ExpCL)			[strict(1)]
	 | #tyExpCL(ExpCL,CoreTy)		[strict(1)]

syntax TP
       ::= CoreTy
         
	 
syntax ValueCL
       ::= #tyval(ValueCL,CoreTy)

syntax ValueCL
       ::= lhsV(Ident)


syntax CLItem
       ::= lHS(ExpCL,ExpCL)	[strict]

syntax ExpCL
       ::= stackwrite(ExpCL,ExpCL)		[seqstrict(2,1)]
	 | heapwrite(ExpCL,Order,ExpCL)		[seqstrict(3,1)]
syntax ExpCL
       ::= lhscl(ExpCL)

syntax Ident
       ::= VarId

syntax VarId
       ::= var( Int )				
         | cvar( Exp, Int)			
	 | #Heap(Exp,CoreTy)

syntax Exp




syntax ExpCL
       ::= #countMem(Int)
         | #countBlock(Map)
	 | #referto(ExpCL,ExpCL)	[strict]

syntax Bool
       ::= #inSet(K,Set)    [function, smtlib(smt_set_mem)]

syntax Set
       ::= #list2Set(List)		[function]


syntax Int
       ::= #cThread(Int,Int)		[function]


rule #cThread(I:Int, I:Int) => I
rule #cThread(-1, I:Int) => I


endmodule
