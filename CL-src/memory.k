
require "memory-syntax.k"
require "configuration.k"

module MEMORY

import MEMORY-SYNTAX
import CONFIGURATION


syntax Map
       ::= #CStoreF(Int,Int,Int,Int,ValueCLs)	   [function]

rule #CStoreF(A:Int,I:Int,N:Int, CLK, V, Vs) =>
                        (#heap(A,I) |-> #lo(CLK, V)
			 #CStoreF(A,I +Int 1, N, CLK, Vs))
     requires I <Int N

rule #CStoreF(A,N1,N,_,.ValueCLs) => .Map
     requires N1 >=Int N

rule <k> #allocate(SIZE:Int, Vs:ValueCLs) => #blk(A)  ... </k>
     <blkAddr> A:Int => A +Int 1 </blkAddr>
     <blocks> .Map => (A |-> #blkInfo(SIZE))
                       #CStoreF(A,0,SIZE,CLK,Vs) ... </blocks>
     <clk> CLK:Int => CLK +Int 1 </clk>

syntax K ::= #lvi(K,Int,Int)

rule <k> #lv(P,FL) => #lvi(P,CLK,FL) ... </k>
     <clk> CLK:Int </clk>

rule <k> #lvi(#stack(L:Int),CLKB:Int,FL:Int) => #pair(#stack(L),CLKB) ... </k>
     <sloc> ... L |-> #lo(CLKO,V) ... </sloc>

syntax Int ::= #RW(Int,Int,Int)	  [function]

rule #RW(0,I,_) => I
rule #RW(1,_,I) => I

rule <k> #allocStack(SI:Int) => #slv(#stack(N)) ...  </k>
     <slocAddr> N:Int => N +Int 1</slocAddr>
     <sloc> SLOC:Map => SLOC[N <- #lo(CLK, #uninit(0))] </sloc>
     <clk> CLK:Int => CLK +Int 1 </clk>


rule M:MemOp Ms:MemOps => M ~> Ms

rule V:ValueCL ~> Ms:MemOps => Ms

rule L:Location ~> Ms:MemOps => Ms

syntax Location
       ::= #lvdref(K,Int)		  [strict]

rule #lvi(#deref(P:Path),CLKB:Int,FL) => #lvdref(#lvi(P,CLKB,FL),FL)

rule <k> #lvdref(#pair(#stack(N:Int),CLKO:Int),FL) =>
			#lvi(P,CLKOO,FL) ...  </k>
     <sloc> ... N |-> #lo(CLKOO => #RW(FL,CLKOO,CLK), #ref(P:Path)) ... </sloc>
     <clk> CLK:Int </clk>
     requires notBool ((CLKO <=Int CLKOO) andBool
                       (CLKOO <=Int CLK))

rule <k> #lvdref(#pair(#heap(N:Int,O:Int),CLKO:Int),FL) =>
                       #lvi(P,CLKOO,FL) ...  </k>
     <blocks> ... #heap(N,O) |-> #lo(CLKOO => #RW(FL,CLKOO,CLK), #ref(P:Path)) ...
     </blocks>
     <clk> CLK:Int </clk>
     requires notBool ( (CLKO <=Int CLKOO) andBool
                        (CLKOO <=Int CLK))

syntax K ::= #lvField(K,Int,Int)  [strict]

rule #lvi(#field(P:Path,N:Int),CLKB:Int,FL:Int)
           => #lvField(#lvi(P,CLKB,FL), N, FL)

rule <k> #lvField(#pair(#heap(A:Int,N:Int),CLKO:Int),H:Int, FL:Int)
               => #pair(#heap(B, H),CLKOO )... </k>
     <blocks> ... #heap(A, N) |-> #lo((CLKOO:Int => #RW(FL,CLKOO,CLK)),#blk(B:Int))
              ... </blocks>
     <clk> CLK:Int </clk>
     requires notBool ((CLKO <=Int CLKOO) andBool
     	      	       (CLKOO <=Int CLK))

rule <k> #lvField(#pair(#stack(L:Int),CLKO:Int),H:Int, FL:Int)
               => #pair(#heap(B, H),CLKOO )... </k>
     <sloc> ... L |-> #lo((CLKOO => #RW(FL,CLKOO,CLK)),#blk(B:Int))
              ... </sloc>
     <clk> CLK:Int </clk>
     requires notBool ((CLKO <=Int CLKOO) andBool
     	      	       (CLKOO <=Int CLK))


syntax K ::= #writei(K,ValueCL)   [strict(1)]
           | #readi(K) [strict(1)]

rule <k> #write(P,V) => #writei(#lv(P,1),V) ~> #unit... </k>

rule <k> #writei(#pair(#stack(L),_),V) => . ... </k>
     <sloc> ... L |-> #lo((_ => CLK), (_ => V)) ...  </sloc>
     <clk> CLK:Int => CLK +Int 1 </clk>

rule <k> #writei(#pair(#heap(A,N),_),V) => . ... </k>
     <blocks> ... #heap(A,N) |-> #lo((_ => CLK), (_ => V)) ...  </blocks>
     <clk> CLK:Int => CLK +Int 1 </clk>


rule <k> #read(P) => #readi(#lv(P,0)) ... </k>

rule <k> #readi(#pair(#stack(L),CLKB)) => V ... </k>
     <sloc> ... L |-> #lo(CLKO,V) ...  </sloc>
     <clk> CLK:Int => CLK +Int 1 </clk>
     requires (notBool ((CLKB <=Int CLKO) andBool (CLKO <=Int CLK)))
     	      andBool (V =/=K #uninit(0))

rule <k> #readi(#pair(#heap(A,N),CLKB)) => V ... </k>
     <blocks> ... #heap(A,N) |-> #lo(CLKO,V) ...  </blocks>
     <clk> CLK:Int => CLK +Int 1 </clk>
     requires (notBool ((CLKB <=Int CLKO) andBool (CLKO <=Int CLK)))
     	      andBool (V =/=K #uninit(0))

syntax K ::= #freeBlk(Int,Int)

rule <k> #free(#blk(A:Int)) => #freeBlk(A,N -Int 1) ... </k>
     <blocks> ... (A |-> #blkInfo(N:Int)) => .Map ... </blocks>
     <clk> CLK:Int => CLK +Int 1 </clk>


rule <k> #freeBlk(A,N) => #freeBlk(A,N -Int 1) ... </k>
     <blocks> ... (#heap(A,N) |-> _) => .Map ... </blocks>
     requires N >Int 0

rule <k> #freeBlk(A,0) => #unit ... </k>
     <blocks> ... (#heap(A,0) |-> _) => .Map ... </blocks>


endmodule