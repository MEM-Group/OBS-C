
require "memory-syntax.k"
require "clang-syntax.k"
require "importcl.k"
require "sortindexcl.k"

module MEMORY

import MEMORY-SYNTAX
import IMPORTCL
import CLANG-SYNTAX
import SORTINDEXCL

/*

syntax KItem
       ::= #CStore(Int,Int)			

syntax Map
       ::= #CStoreF(Int,Int)			 [function]

rule #CStoreF(A:Int,0) => #heap(A,0) |-> #uninit(0)

rule #CStoreF(A:Int,-1) => .Map

rule #CStoreF(A:Int,N:Int) => (#heap(A,N) |-> #uninit(0)) #CStoreF(A,N -Int 1)
     requires N >Int 0


rule <k> #CStore(_,-1) => . ... </k>

rule <k> #CStore(A:Int, I:Int) => #CStore(A, I -Int 1) ... </k>
     <blocks> .Map => #heap(A,I) |-> #uninit(0) ... </blocks>
     requires I >=Int 0

syntax Map
       ::= #CStatus(Int,Int,Int)		[function]

rule #CStatus(B:Int,I:Int,N:Int) => #heap(B,I) |-> #ms(0,0) #CStatus(B,I+Int 1,N)
     requires I <Int N

rule #CStatus(B:Int,I:Int,N:Int) => .Map
     requires I >=Int N


syntax KMEM
       ::= #writeVals(Int,ValueCLs,Int)

rule <k> #writeVals(A:Int, V:ValueCL, Vs:ValueCLs, I:Int)
     		       		  => #Write(#pair(#heap(A,I),(true)),V)
					~> #writeVals(A,Vs,I +Int 1) ... </k>

rule #writeVals(_, .ValueCLs, _) => .K

rule <k> #allocateInit(SIZE:Int,T:CoreTy, Vs:ValueCLs)
                     => #writeVals(A,Vs,0) ~> #own(#blk(A)) ... </k>
     <blkAddr> A:Int => A +Int 1 </blkAddr>
     <blocks> .Map => (A |-> #blkInfo(SIZE,T)) #CStoreF(A,SIZE -Int 1) ... </blocks>
     <memStatus> MEMS:Map => MEMS #CStatus(A, 0, SIZE) </memStatus>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #allocateInitRaw(SIZE:Int,T:CoreTy, Vs:ValueCLs) =>  #writeVals(A,Vs,0) ~> #Raw(#heap(A,0)) ... </k>
     <blkAddr> A:Int => A +Int 1 </blkAddr>
     <blocks> .Map => (A |-> #blkInfo(SIZE,T)) #CStoreF(A,SIZE -Int 1) ... </blocks>
     <memStatus> MEMS:Map => MEMS #CStatus(A, 0, SIZE) </memStatus>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




rule <k> #allocate(SIZE:Int,T:CoreTy) => #Raw(#heap(A,0)) ... </k>
     <blkAddr> A:Int => A +Int 1 </blkAddr>
     <blocks> .Map => (A |-> #blkInfo(SIZE,T)) #CStoreF(A,SIZE -Int 1) </blocks>
     <memStatus> MEMS:Map => MEMS #CStatus(A, 0, SIZE) </memStatus>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

     

rule <k> M:MemOp Ms:MemOps => M ~> Ms ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> V:ValueCL ~> Ms:MemOps => Ms ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> V:ValueCL ~> #writeVals(I,Vs,J) => #writeVals(I,Vs,J) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> S:StackLoc ~> Ms:MemOps => Ms ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #free(#blk(A:Int)) => #freeBlk(A,N -Int 1) ... </k>
     <blocks> ... (A |-> #blkInfo(N:Int,_)) => .Map ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #freeBlk(A,N) => #freeBlk(A,N -Int 1) ... </k>
     <blocks> ... (#heap(A,N) |-> _) => .Map ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
          andBool  N >Int 0

rule <k> #freeBlk(A,0) => #unit ... </k>
     <blocks> ... (#heap(A,0) |-> _) => .Map ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

     

rule <k> #lv(#sl(L:Int)) => #pair(#sl(L),(true)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #lv(#Brw(I:Int,P:Path)) => #Brw(I,P) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



syntax K
       ::= #LVDref(KItem)			[strict]
         | #LV(KItem,KItem)			[strict(1)]

rule <k> #lv(#dRef(P:Path)) =>  #LVDref(#lv(P)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #LVDref(#pair(#sl(L:Int),T:ValueCL))
     	 => #wellFormRef(L,#shr(Lb,CLK,P)) ~> #wellFormAct(#shr(Lb,CLK,P))
	    				      ~> #rdLVDref(#readna(#sl(L)),T)  ... </k>
     <sloc> ... L |-> #shr(Lb:Int,(Le:Int => CLK), P:Path) ... </sloc>
     <clk> CLK:Int </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVDref(#pair(#heap(B:Int,O:Int),T:ValueCL))
     	 => #wellFormRef(#heap(B,O),#shr(Lb,CLK,P)) ~> #wellFormAct(#shr(Lb,CLK,P))
	         ~> #rdLVDref(#readna(#heap(B,O)),T) ... </k>
     <blocks> ... #heap(B,O) |-> #shr(Lb:Int,(Le:Int => CLK), P:Path) ... </blocks>
     <clk> CLK:Int </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVDref(#pair(#sl(L:Int),T:ValueCL))
     	 => #wellFormRef(L,#mut(Lb,CLK,P))
             ~> #wellFormAct(#mut(Lb,CLK,P)) ~> #rdLVDref(#readna(#sl(L)),T)  ... </k>
     <sloc> ... L |-> #mut(Lb:Int,(Le:Int => CLK), P:Path) ... </sloc>
     <clk> CLK:Int </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVDref(#pair(#heap(B:Int,O:Int),T:ValueCL))
     	 => #wellFormRef(#heap(B,O),#mut(Lb,CLK,P)) ~> #wellFormAct(#shr(Lb,CLK,P))
	       ~> #rdLVDref(#readna(#heap(B,O)),T) ... </k>
     <blocks> ... #heap(B,O) |-> #mut(Lb:Int,(Le:Int => CLK), P:Path) ... </blocks>
     <clk> CLK:Int </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVDref(#pair(#sl(L:Int),T:ValueCL)) => #stuck("LValue Uninit", #sl(L:Int)) ... </k>
     <sloc> ... L |-> #uninit(_) ... </sloc>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVDref(#pair(#heap(B:Int,O:Int),T:ValueCL)) =>
     	 			#stuck("LValue Uninit", #heap(B:Int,O:Int)) ... </k>
     <blocks> ... #heap(B,O) |-> #uninit(_) ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVDref(#pair(#sl(L:Int),T:ValueCL)) => #rdLVDref(#readna(#sl(L)), T) ... </k>
     <sloc> ... L |-> #Raw(L1) ... </sloc>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVDref(#pair(L,T:ValueCL)) => #rdLVDref(#readna(L), T) ... </k>
     <blocks> ... L |-> #Raw(L1) ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax KItem
       ::= #rdLVDref(KItem, KItem)	[strict(1)]


rule <k> #rdLVDref(#Raw(L),T:ValueCL) => #pair(L,T) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #rdLVDref(#shr(Lb:Int,Le:Int,P:Path), T:ValueCL) =>
     	 #LV(#lv(P),false) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #rdLVDref(#Shr(P:Path), T:ValueCL) =>
     	 #LV(#lv(P),false) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #rdLVDref(#mut(Lb:Int,Le:Int,P:Path), T:ValueCL) =>
     	 #LV(#lv(P),true) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #rdLVDref(#Mut(P:Path), T:ValueCL) =>
     	 #LV(#lv(P),true) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




rule <k> #LV(#pair(L:KItem,T), T1) => #pair(L,T andBool T1) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax K
       ::= #LVField(KItem,KItem)	[strict]

rule <k> #lv(#PFieldExpCL(P:Path,I:ExpCL)) => #LVField(#lv(P),I) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #lv(#PArray(P:Path,E:ExpCL)) => #LVField(#lv(P),E) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax KItem
       ::= #rdLVfield(KItem,ValueCL,ValueCL)		[strict(1)]

rule <k> #rdLVfield(#own(#blk(B:Int)), T:Bool, I:Int) =>
     #pair(#heap(B,I),T) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #rdLVfield(#Raw(#heap(B:Int, I1:Int)), T:Bool, (I:Int)) =>
     #pair(#heap(B,I1 +Int I),(T)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #LVField(#pair(L:Location, (T:Bool)), (I:Int))
     	 			   => #rdLVfield(#readna(L),(T),(I)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #wellFormRef(L:K,V) => #ckRefLT(SLOC,L,V) ~> #ckRefLT(BLK,L,V) ... </k>
     <sloc> SLOC:Map </sloc>
     <blocks> BLK:Map </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




rule <k> #wellFormAct(R:K) => #ckAct(ACT,R) ... </k>
     <locRW> ACT:Set </locRW>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ckAct(SetItem(#lread(_,_)) Rest:Set, #shr(Lb:Int,Le:Int,P:Path))
     				 => #ckAct(Rest, #shr(Lb,Le,P))  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1





rule <k> #ckAct(SetItem(#lwrite(P1:Path,L:Int)) Rest:Set, #shr(Lb:Int,Le:Int,P:Path))
     => #selectE((#match(P1,P) andBool ((Lb <=Int L) andBool (L <=Int Le))),
	      #stuck(#lwrite(P1,L), #shr(Lb,Le,P)),
              #ckAct(Rest, #shr(Lb,Le,P))
	      ) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule #ckAct(SetItem(#lwrite(P1,L:Int)) RestS:Set, #mut(Lb:Int,Le:Int,P:Path))
     				=> #ckAct(RestS, #mut(Lb,Le,P))
     requires (notBool #match(P1,P))

rule #ckAct(SetItem(#lwrite(P1,L:Int)) Rest:Set, #mut(Lb:Int,Le:Int,P:Path))
     				=> #ckAct(Rest, #mut(Lb,Le,P))
     requires (#match(P1,P) andBool ((L <Int Lb) orBool (L >Int Le)))

rule #ckAct(SetItem(#lwrite(P1,L:Int)) Rest:Set, #mut(Lb:Int,Le:Int,P:Path))
     				=> #stuck(#lwrite(P1,L), #mut(Lb,Le,P))
     requires (#match(P1,P) andBool ((Lb <=Int L) andBool (L <=Int Le)))

rule #ckAct(SetItem(#lread(P1,L:Int)) Rest:Set, #mut(Lb:Int,Le:Int,P:Path))
     				=> #ckAct(Rest, #mut(Lb,Le,P))
     requires (notBool #match(P1,P))

rule #ckAct(SetItem(#lread(P1,L:Int)) Rest:Set, #mut(Lb:Int,Le:Int,P:Path))
     				=> #ckAct(Rest, #mut(Lb,Le,P))
     requires (#match(P1,P) andBool ((L <Int Lb) orBool (L >Int Le)))

rule #ckAct(SetItem(#lwrite(P1,L:Int)) Rest:Set, #mut(Lb:Int,Le:Int,P:Path))
     				=> #stuck(#lwrite(P1,L),#shr(Lb,Le,P))
     requires (#match(P1,P) andBool ((Lb <=Int L) andBool (L <=Int Le)))

rule #ckAct(.Set,_) => .K



syntax KItem
       ::= #wellFormRef(K,K)
         | #wellFormAct(K)
	 | #ckAct(Set,K)				  	        
         | #ckRefLT(Map,K,K)				
	 | #ckTwoRef(K,K)				


rule #selectE(true,E1:K,_) => E1
rule #selectE(false,_,E1:K) => E1

rule <k> #ckRefLT((Key |-> V) ENV:Map, L:K, V1:K) => #selectE(Key ==K L,
				#ckRefLT(ENV,L,V1),
				#ckTwoRef(V,V1) ~> #ckRefLT(ENV, L, V1)
				) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #ckRefLT(.Map, _, _) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ckTwoRef((_:Int),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef((_:Bool),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef((_:String),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef((_:Float),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef(#uninit(_),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef(#own(_),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef(#Raw(_),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef(#shr(_,_,_),#shr(_,_,_)) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef(#blkInfo(_,_),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef(cr(_),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #ckTwoRef(#threadId(_),_) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #ckTwoRef(#shr(L1,L2,P1),#mut(L3,L4,P2)) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool (notBool #match(P1,P2))

rule <k> #ckTwoRef(#shr(L1,L2,P1),#mut(L3,L4,P2)) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool (#match(P1,P2) andBool (notBool #lfck(L1,L2,L3,L4)))

rule <k> #ckTwoRef(#shr(L1:Int,L2:Int,P1:Path),#mut(L3:Int,L4:Int,P2:Path)) 
            => #stuck(#mut(L1,L2,P1),#shr(L3,L4,P2)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool ( #match(P1,P2) andBool (#lfck(L1,L2,L3,L4)))

rule <k> #ckTwoRef(#mut(L1:Int,L2:Int,P1:Path),#shr(L3:Int,L4:Int,P2:Path)) =>
          #selectE( #match(P1,P2) andBool (#lfck(L1,L2,L3,L4)),
	            #stuck(#mut(L1,L2,P1),#shr(L3,L4,P2)), .K)
          ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)


syntax K
       ::= #stuck(K,K)

rule <k> #ckTwoRef(#mut(L1,L2,P1),#mut(L3,L4,P2)) =>
        #selectE(#match(P1,P2) andBool (#lfck(L1,L2,L3,L4)),
	         #stuck(#mut(L1,L2,P1),#mut(L3,L4,P2)),
		 .K) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)

rule <k> #ckTwoRef(#shr(L1,L2,P1),#mut(L3,L4,P2)) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool ( notBool (#match(P1,P2)))

rule <k> #ckTwoRef(#shr(L1,L2,P1),#mut(L3,L4,P2)) => .K ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool (#match(P1,P2) andBool (notBool #lfck(L1,L2,L3,L4)))

rule <k> #ckTwoRef(#shr(L1:Int,L2:Int,P1:Path),#mut(L3:Int,L4:Int,P2:Path))
				=> #stuck(#shr(L1,L2,P1),#mut(L3,L4,P2)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool (#match(P1,P2) andBool (#lfck(L1,L2,L3,L4)))



     

syntax Bool
       ::= #match(Path,Path)		[function]       

syntax Int
       ::= #Len(Path)			[function]

syntax Path
       ::= #cut(Path, Int)		[function]

rule #Len(#PFieldExpCL(P:Path, (I:Int))) => #Len(P) +Int 1
rule #Len(L:StackLoc) => 0

rule #cut(#PFieldExpCL(P:Path,(I:Int)), N:Int) => #cut(P,N -Int 1)
     requires N >Int 0

rule #cut(P:Path,0) => P

rule #match(P1:Path, P2:Path) => P2 ==K #cut(P1,#Len(P1) -Int #Len(P2))
     requires #Len(P1) >=Int #Len(P2)

rule #match(P1:Path, P2:Path) => P1 ==K #cut(P2,#Len(P2) -Int #Len(P1))
     requires #Len(P2) >Int #Len(P1)


       



syntax Bool
       ::= #lfck(Int,Int,Int,Int)		[function]

rule #lfck(L1:Int,L2:Int,B1:Int,B2:Int) =>
     		((L1 <=Int B1) andBool (B1 <=Int L2)) orBool
		((L1 <=Int B2) andBool (B2 <=Int L2)) orBool
		((B1 <=Int L1) andBool (L1 <=Int B2)) orBool
		((B1 <=Int L2) andBool (L2 <=Int B2))






rule <k> #allocStack => #sl(N) ...  </k>
     <slocAddr> N:Int => N +Int 1</slocAddr>
     <sloc> SLOC:Map => SLOC[N <- #uninit(0)] </sloc>


syntax InterTerm
       ::= #Write(KItem,WriteTerm)			[strict]
       
rule <k> #write(P:Path, V:WriteTerm) => #Write(#lv(P),V) ... </k>
     <locRW> .Set => SetItem(#lwrite(#route(P),CLK)) ... </locRW>
     <clk> CLK:Int </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #Write(#pair(L:Location,(true)), V:WriteTerm)
     	    => #writena(L,#clkVal(V,CLK)) ... </k>
     <clk> CLK:Int => CLK +Int 1 </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax K
       ::= #Read(KItem)				[strict]



rule <k> #read(P:Path) => #Read(#lv(P)) ... </k>
     <locRW> .Set => SetItem(#lread(#route(P),CLK)) ... </locRW>
     <clk> CLK:Int </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #Read(#pair(#sl(L:Int),(T:Bool)))
		=> #ValRead(L,V,CLK) ... </k>
     <sloc> ... L |-> (V:ValueCL => #clkVal(V,CLK)) ... </sloc>
     <clk> CLK:Int => CLK +Int 1 </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #Read(#pair(#heap(B:Int,O:Int),(T:Bool)))
		=> #ValRead(#heap(B,O),V,CLK) ... </k>
     <blocks> ... #heap(B,O) |-> V:ValueCL ... </blocks>
     <clk> CLK:Int => CLK +Int 1 </clk>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




rule #Read(#Shr(V)) => #Shr(V)
rule #Read(#Mut(V)) => #Mut(V)




syntax KItem
      ::= #ValRead(K,K,Int)
        | #processVal(K)


rule <k> #ValRead(L:Int,#shr(Lb,Le,P),CLK:Int) => #wellFormRef(L,#shr(Lb,CLK,P))
     				       ~> #wellFormAct(#shr(Lb,CLK,P)) ~> #readna(#sl(L)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ValRead(L:Int,#mut(Lb,Le,P),CLK:Int) => #wellFormRef(L,#mut(Lb,CLK,P))
     				       ~> #wellFormAct(#mut(Lb,CLK,P)) ~> #readna(#sl(L)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ValRead(#heap(B:Int,O:Int),#shr(Lb,Le,P),CLK:Int) => #wellFormRef(#heap(B,O),#shr(Lb,CLK,P))
     				       ~> #wellFormAct(#shr(Lb,CLK,P)) ~> #readna(#heap(B,O)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ValRead(#heap(B:Int,O:Int),#mut(Lb,Le,P),CLK:Int) => #wellFormRef(#heap(B,O),#mut(Lb,CLK,P))
     				       ~> #wellFormAct(#mut(Lb,CLK,P)) ~> #readna(#heap(B,O)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #ValRead(L:Int,V:ValueCL,_) => #readna(#sl(L)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
      andBool ((termIndex(V) =/=Int 100018) andBool
     	      (termIndex(V) =/=Int 100019))

rule <k> #ValRead(#heap(B:Int,O:Int),V:ValueCL,_) => #readna(#heap(B,O)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
      andBool ((termIndex(V) =/=Int 100018) andBool
     	      (termIndex(V) =/=Int 100019))


rule #processVal(#shr(Lb,Le,P)) => #Shr(P)


rule #processVal(#mut(Lb,Le,P)) => #Mut(P)


rule #processVal(V:ValueCL) => V
     requires (termIndex(V) =/=Int 100018) andBool
     	      (termIndex(V) =/=Int 100019)










syntax Path
       ::= #route(Path)			[function]

rule #route(#sl(A:Int)) => #sl(A)
rule #route(#dRef(P:Path)) => #route(P)
rule #route(#PFieldExpCL(#dRef(P:Path), (I:Int))) => #route(P)
rule #route(#PFieldExpCL(#sl(A:Int), (I:Int))) => #sl(A) . (I)

rule #route(#PFieldExpCL(#PFieldExpCL(P:Path,(I:Int)),(J:Int)))
	=> #PFieldExpCL(#route(#PFieldExpCL(P,(I))),(J))

rule #route(#PArray(P:Path,E:ExpCL)) => #route(P)


rule #clkVal((A:Int),_) =>  (A)
rule #clkVal((A:Bool),_) =>  (A)
rule #clkVal((A:String),_) =>  (A)
rule #clkVal((A:Float),_) =>  (A)
rule #clkVal(#own(B:BlkAddr),_) =>  #own(B)
rule #clkVal(#Raw(B),_) =>  #Raw(B)
rule #clkVal(#uninit(0),_) =>  #uninit(0)
rule #clkVal(#Shr(P:Path),CLK:Int) =>  #shr(CLK,CLK,P)
rule #clkVal(#Mut(P:Path),CLK:Int) =>  #mut(CLK,CLK,P)
rule #clkVal(#threadId(I),CLK:Int) =>  #threadId(I)

rule #clkVal(#shr(Lb,_,P:Path),CLK:Int) =>  #shr(Lb,CLK,P)
rule #clkVal(#mut(Lb,_,P:Path),CLK:Int) =>  #mut(Lb,CLK,P)
rule #clkVal(cr(L),CLK:Int) =>  cr(L)


rule #Brw(N:Int,P:Path) => #brwK(N,#brw(P))

rule #brwK(N:Int,#val(P:Path)) => #CRef(N,P)

syntax KItem
       ::= #brwK(Int,KItem)			[strict(2)]
	 | #brw(KItem)
         | #brwDref(KItem)			[strict]
	 | #brwField(KItem,KItem)		[strict(1,2)]
	 | #brwArray(KItem,KItem)		[strict(1,2)]			


rule #brw(#sl(I:Int)) => #val(#sl(I))

rule <k> #brwDref(#sl(L:Int))
     	 => #stuck("Error", #brwDref(#sl(L:Int)))  ... </k>
     <sloc> ... L |-> #uninit(_) ... </sloc>

rule #brw(#dRef(P:Path)) => #brwDref(#read(P))
rule #brwDref(#Mut(#sl(L:Int))) => #val(#sl(L))
rule #brwDref(#Shr(#sl(L:Int))) => #val(#sl(L))

rule #brw(#PFieldExpCL(P:Path,(I:Int))) => #brwField(#brw(P),(I))

rule #brwField(#val(P:Path),(I:Int)) => #val(#PFieldExpCL(P,(I)))

rule #brw(#PArray(P:Path,E:ExpCL)) => #brwArray(#brw(P),E)
rule #brwArray(#val(P:Path), (I:Int)) => #val(#PArray(P,(I)))


syntax K
       ::= #CRef(Int,Path)			[function]

rule #CRef(0,P:Path) => #Shr(P)
rule #CRef(1,P:Path) => #Mut(P)

syntax K
       ::= #assignT(KItem,KItem)			[strict(2)]

rule #Assign(P1:Path,P2:Path) => #assignT(P1,#read(P2))
rule #assignT(P:Path,V:ValueCL) => #Write(#lv(P),V)


     
rule <k> #readna(#sl(L:Int)) => #readnap(#sl(L)) ...  </k>
     <threadId> TID:Int </threadId>
     <memStatus> ... #sl(L) |-> #ms((N1:Int => N1 +Int 1), 0) ...  </memStatus>
     <cthread> CTID:Int => -1 </cthread>
     requires ((TID ==Int CTID) orBool(TID ==Int -1))

rule <k> #readna(#sl(L:Int)) => #stuck("readna", #sl(L)) ... </k>
     <threadId> TID:Int </threadId>
     <memStatus> ... #sl(L) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2))) ...  </memStatus>
     <cthread> CTID:Int => -1 </cthread>
     requires (N2 =/=Int 0) andBool ((TID ==Int CTID) orBool(TID ==Int -1))

rule <k> #readnap(#sl(L:Int)) => #processVal(V)  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <sloc> ... L |-> V:ValueCL ... </sloc>
     <memStatus> ... #sl(L) |-> #ms((N1:Int => N1 -Int 1), 0) ... </memStatus>


rule <k> #readnap(#sl(L:Int)) => #stuck("readnap", #sl(L:Int))  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <memStatus> ... #sl(L) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2))) ... </memStatus>
     requires N2 =/=Int 0



rule <k> #readna(#heap(B:Int,N:Int)) =>  #readnap(#heap(B,N)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID => -1 </cthread>
     <memStatus> ... #heap(B,N) |-> #ms((N1:Int => N1 +Int 1), 0) ...  </memStatus>
     requires ((TID ==Int CTID) orBool(TID ==Int -1))


rule <k> #readna(#heap(B:Int,N:Int)) => #stuck("readna", #heap(B,N)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID => -1 </cthread>
     <memStatus> ... #heap(B,N) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2))) ...  </memStatus>
     requires (N2 =/=Int 0) andBool
              ((TID ==Int CTID) orBool(TID ==Int -1))


rule <k> #readnap(#heap(B:Int,N:Int)) => #processVal(V)  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <blocks> ... #heap(B,N) |-> V:ValueCL ... </blocks>
     <memStatus> ... #heap(B,N) |-> #ms((N1:Int => N1 -Int 1), 0) ... </memStatus>

rule <k> #readnap(#heap(B:Int,N:Int)) => #stuck("readnap", #heap(B:Int,N:Int))  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <memStatus> ... #heap(B,N) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2))) ... </memStatus>
     requires N2 =/=Int 0


rule <k> #writena(#heap(B:Int,N:Int),V:WriteTerm) => #writenap(#heap(B,N),V) ... </k>
     <threadId> TID:Int </threadId>
     <memStatus> ... #heap(B,N) |-> #ms(0,(0 => 1)) ... </memStatus>
     <cthread> CTID:Int => -1 </cthread>
     requires ((TID ==Int CTID) orBool(TID ==Int -1))


rule <k> #writena(#heap(B:Int,N:Int),V:WriteTerm) => #stuck("writena", #heap(B,N)) ... </k>
     <threadId> TID:Int </threadId>
     <memStatus> ... #heap(B,N) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2)))  ... </memStatus>
     <cthread> CTID => -1 </cthread>
     requires ((N1 =/=Int 0) orBool (N2 =/=Int 0)) andBool
     	      ((TID ==Int CTID) orBool(TID ==Int -1))

rule <k> #writenap(#heap(B:Int,N:Int),V:WriteTerm) => #unit  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <blocks> ... #heap(B,N) |-> (_ => V) ... </blocks>
     <memStatus> ... #heap(B,N) |-> #ms(0,(1 => 0)) ... </memStatus>


rule <k> #writenap(#heap(B:Int,N:Int),V:WriteTerm)
		=> #stuck("writenap",#heap(B:Int,N:Int))  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <memStatus> ... #heap(B,N) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2))) ... </memStatus>
     requires (N1 =/=Int 0) orBool (N2 =/=Int 1)


rule <k> #writena(#sl(L:Int),V:WriteTerm) => #writenap(#sl(L),V) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => -1 </cthread>
     <memStatus> ... #sl(L) |-> #ms(0,(0 => 1)) ... </memStatus>
     requires ((TID ==Int CTID) orBool(TID ==Int -1))


rule <k> #writena(#sl(L:Int),V:WriteTerm) => #stuck("writena", #sl(L)) ... </k>
     <threadId> TID:Int </threadId>
     <memStatus> ... #sl(L) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2))) ... </memStatus>
     <cthread> CTID:Int => -1 </cthread>
     requires ((N1 =/=Int 0) orBool (N2 =/=Int 0)) andBool
              ((TID ==Int CTID) orBool(TID ==Int -1))


rule <k> #writenap(#sl(L:Int),V:WriteTerm) => #unit  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <sloc> ... L |-> (_ => V) ... </sloc>
     <memStatus> ... #sl(L) |-> #ms(0,(1 => 0)) ... </memStatus>


rule <k> #writenap(#sl(L:Int),V:WriteTerm) => #stuck("writenap",#sl(L:Int))  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> -1 => TID </cthread>
     <memStatus> ... #sl(L) |-> (#ms(N1:Int,N2:Int) => #lockS(#ms(N1,N2)))... </memStatus>
     requires (N1 =/=Int 0) orBool (N2 =/=Int 1)



syntax Map
       ::= #select1(Bool,Map,K)		[function]
          |#select2(Bool,Map,K)		[function]


rule #vecDequeMap(A:Int,N:Int) => (#heap(A,N) |-> #uninit(0)) #vecDequeMap(A,N -Int 1)
     requires N >=Int 0

rule #vecDequeMap(A:Int,-1) => .Map

*/


endmodule