


module VEC-PATTERN

import DOMAINS
imports MEMORY-SYNTAX
imports CONFIGURATION
imports CLANG-SYNTAX

/*
syntax ExpCLs
       ::= #List2ExpCLs(List)	[function]

rule #List2ExpCLs(.List) => .ExpCLs
rule #List2ExpCLs(ListItem(I:Int) RestL:List) => #ExpCLs((I), #List2ExpCLs(RestL))


syntax CTList
       ::= #ctlist(Int,CTList)	
       	 | ".CTList"

syntax Int
       ::= #ctlen(CTList)		[function, smtlib(smt_ctlist_len), lemma] 
         | #sumct(CTList)		[function, smtlib(smt_ctlist_sumct), lemma]

rule #sumct(.CTList) => 0
rule #sumct(#ctlist(I:Int,C:CTList)) => I +Int #sumct(C)

rule #ctlen(.CTList) => 0
rule #ctlen(#ctlist(I:Int,C:CTList)) => 1 +Int #ctlen(C)

syntax Map ::= #ctMap(Int,CTList)	     	[pattern(1),function]
       	     | #ctMapB(Int,Int,CTList)    [pattern(1),function]


rule #ctMap(B:Int,C:CTList) => B |-> #blkInfo(#ctlen(C),#vec(i32))
		               #ctMapB(B,0,C)
		   [pattern]

rule #ctMapB(B:Int,O:Int,#ctlist(I:Int,C:CTList)) => #heap(B,O) |-> (I) #ctMapB(B,O +Int 1, C)
		   [pattern]

rule #ctMapB(_,_,.CTList) => .Map




syntax Map ::= "vecMap" "(" Int ")" "(" List  ")"				[function]


syntax Map ::= "vecMapB" "(" Int "," Int  ")" "(" List  ")"			[function]


rule vecMap(B:Int)(L:List)
                   => B |-> #blkInfo(size(L),#vec(i32))
		      vecMapB(B,0)(L)
		   [pattern]

rule vecMapB(B:Int,O:Int)(ListItem(I:Int) RestL:List) =>
     	#heap(B,O) |-> I
	vecMapB(B,O +Int 1)(RestL)
//	[pattern]

rule vecMapB(B:Int,O:Int)(.List) => .Map
//     [pattern]

*/




endmodule


