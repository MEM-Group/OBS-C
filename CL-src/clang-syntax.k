
require "memory-syntax.k"
require "configuration.k"



module CLANG-SYNTAX

imports MEMORY-SYNTAX
imports CONFIGURATION
imports DOMAINS
imports DOMAINS-SYNTAX


syntax ExpCL
       ::= "if" ExpCL "then" ExpCL "else" ExpCL		[klabel(ifexpcl)]
	 | "let" Ident "=" ExpCL			[klabel(#letcl), strict(2)]
	 | ExpCL ";" ExpCL				[right,klabel(seqexpcl)]
         | Ident
         | ValueCL
	 | MemOp
	 | "*" ExpCL					[klabel(dereferencecl)]
	 | REF(ExpCL)
	 | ExpCL "." ExpCL				[klabel(fieldcl), strict(2)]
	 | call(Ident, ExpCLs)	 		        [klabel(funapplycl), strict(2)]
	 | ArithExpCL	   		
	 | ExpCL ":=" ExpCL			        [klabel(assignExpcl), strict(2)]
	 | FnCL
	 | "{" ExpCL "}"			        [klabel(blockcl)]
	 | ( ExpCL )				        [bracket]
	 | "assert" "(" ExpCL ")"		        [klabel(#assertcl), strict]
	 | "clskip"
	 | move(ExpCL)
	 | panic(ExpCL)			                [strict]
	 | print(ExpCL) 			        [strict]
	 | endThread(Int)
	 | "loop" ExpCL "{" ExpCL "}"		        [klabel(#loopcl)]
	 | "break"
	 | wait(ExpCL)				        [strict]
	 | deallocate(ExpCL)
	 | "destructThread"
	 | forkcl(ExpCL)		                [strict]
	 | "case" ExpCL "of" "{" ExpCLs "}"	        [strict(1),klabel(#casecl1)]
	 | "case" "{" GuardedActions  "}"	        [klabel(#casecl2)]

syntax Ident ::= Id

syntax GuardedActions
       ::= List{GuardedAction, ";"}		[klabel(#guardedActions)]


syntax GuardedAction
       ::= ExpCL "->" ExpCL			[klabel(#guardedAction)]



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


syntax FnCL
       ::= "fun" Ident "(" Idents ")" "{" ExpCL "}"


syntax Idents
       ::= List{Ident, ","}			[klabel(identscl)]


syntax ExpCLs
       ::= List{ExpCL, ","}			[strict,klabel(#ExpCLs)]


syntax ExpCLs
       ::= ValueCLs



endmodule
