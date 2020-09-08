
require "clang-syntax.k"
require "memory.k"




module CLANG

import CONFIGURATION
imports MEMORY
imports DOMAINS
imports FLOAT
imports COLLECTIONS
imports CLANG-SYNTAX

rule <k> X:Ident => #read(L) ... </k>
     <env> ... X |-> L ... </env>

syntax K ::= #letItem(K,Ident,ValueCL)   [strict(1)]

rule let X:Ident = V:ValueCL => #letItem(#allocStack(0),X,V)

rule <k> #letItem(#slv(#stack(L:Int)), X:Ident, V:ValueCL) => #write(#stack(L),V) ... </k>
     <env> ENV:Map => ENV[X <- #stack(L)]   </env>
     <clstack> .List => ListItem(X) ... </clstack>

rule <k> E:ExpCL := V:ValueCL => #write(#replaceVar(E,ENV),V) ... </k>
     <env> ENV </env>

rule E1:ExpCL ; E2:ExpCL => E1 ~> E2

rule <k> * E:ExpCL => #read(#replaceVar(* E, ENV)) ... </k> 
     <env> ENV:Map </env>

rule <k> E:ExpCL . I:Int => #read(#replaceVar(E . I, ENV)) ... </k> 
     <env> ENV:Map </env>

syntax Path
       ::= #replaceVar(ExpCL,Map)		[function]
         | #toPath(K)				[function]

rule #toPath(P:Path) => P
rule #replaceVar(X:Ident, M:Map) => #toPath(M[X])
rule #replaceVar(E1:ExpCL . I:Int, M:Map) => #field(#replaceVar(E1,M), I)
rule #replaceVar(* E:ExpCL, M:Map) => #deref(#replaceVar(E,M))

rule <k> REF(E:ExpCL) => #ref(#replaceVar(E,ENV)) ... </k>
     <env> ENV:Map </env>

rule _:ValueCL ~> E:ExpCL => E

syntax CaseItem
       ::= "#caseItem2" "(" ExpCL ";" ExpCL ";" GuardedActions ")"              [strict(1)]
         | #caseItem(Bool,ExpCLs)

rule case (I) of { EL:ExpCLs}  => #caseItem(I, EL)
rule #caseItem(true, _:ExpCL,EL:ExpCLs) => #caseItem(false, EL)
rule #caseItem(false, E:ExpCL,EL:ExpCLs) => E
rule #caseItem(true, #ValueCLs(E:ValueCL, EL:ValueCLs)) => #caseItem(false,EL)
rule #caseItem(false,#ValueCLs(E:ValueCL, EL:ValueCLs)) => E 


rule case { G:ExpCL -> E:ExpCL ; GAs:GuardedActions } => #caseItem2(G;E;GAs)
rule #caseItem2(true; E; GAs:GuardedActions) => E 
rule #caseItem2(false; E; GAs:GuardedActions) => case {GAs}
rule if E:ExpCL then E1:ExpCL else E2:ExpCL => case E of {E2, E1} 




/********* Function ***********/

rule <k> fun F:Ident ( P:Idents ) { E:ExpCL } => cr(C) ... </k>
    (.Bag =>
        <closure>
           <crId> C </crId>
           <crContext> .K </crContext>
           <crParams> P </crParams>
           <crBody> E </crBody>
        </closure>
     )
     <env> Rho:Map </env>
     <closureCnt> C:Int => C +Int 1 </closureCnt>
     <funclosure> .Map => F |-> C ...  </funclosure>

syntax KItem
       ::= #computeFB(KItem)			[strict]
         | #state(K, Map)
	 | fnCalls(Int, Idents, ValueCLs)
	 | InitParam(Ident,ValueCL)

rule <k> call(F, VL:ValueCLs) ~> RestPGM:K => fnCalls(C,P,VL) </k>
     <funclosure> ... F |-> C ...  </funclosure>
     <crId> C </crId>
     <crParams> P:Idents </crParams>
     <env> Rho:Map </env>
     <clstack> .List => (ListItem(#state(RestPGM, Rho))) ... </clstack>


rule <k> { E:ExpCL } ~> RestPGM:K => #computeFB(E) </k>
     <env> Rho:Map </env>
     <clstack> .List => (ListItem(#state(RestPGM, Rho))) ... </clstack>


rule <k> fnCalls(I:Int, (X:Ident, RestP:Idents), V:ValueCL, RestVl:ValueCLs) =>
         InitParam(X,V) ~> fnCalls(I,RestP,RestVl) ... </k>


rule InitParam(X:Ident, V:ValueCL) => #letItem(#allocStack(0),X,V)

rule #unit ~> fnCalls(X,P,V) => fnCalls(X,P,V)


rule <k> fnCalls(I:Int, .Idents, .ValueCLs) => #computeFB(B) ... </k>
     <crId> I </crId>
     <crBody> B:ExpCL </crBody>


rule <k> #computeFB(V:ValueCL) => V ~> RestPGM ... </k>
     <env> _ => Rho </env>
     <clstack> ListItem(#state(RestPGM, Rho:Map)) => .List ... </clstack>


rule <k> #computeFB(V:ValueCL) => V ... </k>
     <env> _ => Rho </env>
     <clstack> ListItem(Rho:Map) => .List ... </clstack>

rule <k> #unit ~> #computeFB(V) => #computeFB(V) ... </k>

rule <k> #computeFB(V:ValueCL) => #computeFB(V) ... </k>
     <env> ... ((X |-> #stack(L)) => .Map) ...  </env>
     <clstack> ListItem(X:Ident) => .List ... </clstack>
     <sloc> ... ((L |-> V1) => .Map) ... </sloc>

rule #ExpCLs(V:ValueCL, VL:ValueCLs) => #ValueCLs(V, VL)
rule <k> .ExpCLs => .ValueCLs ... </k>

rule <k> print(V:ValueCL) => #unit ... </k>
     <output> .List => ListItem(V) ... </output>

rule <k> (X:Int) ^ (Y:Int) => (X ^Int Y) ... </k>

rule <k> (X:Int) - (Y:Int) => (X -Int Y) ... </k>
rule <k> (X:Float) - (Y:Float) => (X -Float Y) ... </k>

rule <k> (X:Int) + (Y:Int) => (X +Int Y) ... </k>
rule <k> (X:Float) + (Y:Float) => (X +Float Y) ... </k>

rule <k> (X:Int) ** (Y:Int) => (X *Int Y) ... </k>
rule <k> (X:Float) ** (Y:Float) => (X *Float Y) ... </k>

rule <k> (X:Float) / (Y:Float) => (X /Float Y) ... </k>
rule <k> (X:Int) / (Y:Int) => (X /Int Y) ... </k>

rule <k> (X:Int) <= (Y:Int) => (X <=Int Y) ... </k>
rule <k> (X:Int) < (Y:Int) => (X <Int Y) ... </k>
rule <k> (X:Int) == (Y:Int) => (X ==Int Y) ... </k>
rule <k> (X:String) == (Y:String) => (X ==String Y) ... </k>
rule <k> (X:Int) > (Y:Int) => (X >Int Y) ... </k>
rule <k> (X:Int) >= (Y:Int) => (X >=Int Y) ... </k>
rule <k> (X:Int) mod (Y:Int) => (X modInt Y) ... </k>

rule <k> (X:Float) <= (Y:Float) => (X <=Float Y) ... </k>
rule <k> (X:Float) < (Y:Float) => (X <Float Y) ... </k>
rule <k> (X:Float) == (Y:Float) => (X ==Float Y) ... </k>
rule <k> (X:String) == (Y:String) => (X ==String Y) ... </k>
rule <k> (X:Float) > (Y:Float) => (X >Float Y) ... </k>
rule <k> (X:Float) >= (Y:Float) => (X >=Float Y) ... </k>


rule <k> (X:Bool) && (Y:Bool) => (X andBool Y) ... </k>
rule <k> (X:Bool) || (Y:Bool) => (X  orBool Y) ... </k>
rule <k> (X:Int) & (Y:Int) => (X &Int Y) ... </k>

rule assert(true) => #unit 

/*






rule { E:ExpCL } => E





syntax K
       ::= #DESTRUCT(ValueCL)
         | #destruct(Bool,K)

rule <k> #DESTRUCT(V:ValueCL) => #destruct(termIndex(V) ==Int 100016, V) ... </k>


rule <k> #destruct(true,#own(D)) => #free(D) ... </k>

rule <k> #destruct(false,_) => . ... </k>

rule <k> E1:ExpCL . E2:ExpCL  => #read(#replaceVar(E1 . E2, ENV)) ... </k> 
     <env> ENV:Map </env>


rule <k> * E:ExpCL => #read(#replaceVar(* E, ENV)) ... </k> 
     <env> ENV:Map </env>


 

rule <k> fun ( P:Idents ) { E:ExpCL } => cr(C) ... </k>
     (.Bag =>
        <closure>
           <crId> C </crId>
           <crContext> .K </crContext>
           <crParams> P </crParams>
           <crBody> E </crBody>
        </closure>
     )
     <closureCnt> C:Int => C +Int 1 </closureCnt>



     
rule <k> V:ValueCL ~> E2:ExpCL => #selectE(V =/=K #break, E2, .K) ... </k>
     

rule <k> #fname(F:Ident) => cr(I) ... </k>
     <funclosure>  ... (F |-> I:Int) ...  </funclosure>
     

rule <k> #fname(F:Ident) => F ... </k>
     <funclosure> Rho:Map  </funclosure>
     
rule <k> #fname(cr(I:Int)) => cr(I) ... </k>
     

rule <k> #fname(E1:ExpCL . E2:ExpCL) => E1 . E2 ... </k>
     

rule <k> #fname(* E:ExpCL) => * E ... </k>
     


rule #inSet(E:KItem, SetItem(E) Rest:Set) => true

rule #inSet(E:K, .Set) => false
rule #inSet(E:K, SetItem(E1) Rest:Set) => #inSet(E,Rest)
     requires E =/=K E1




syntax FName
       ::= #fname(ExpCL)

syntax ExpCL
       ::= #callcl(KItem, ExpCLs)		[strict]

rule <k> callcl(F:ExpCL,Es:ExpCLs) => #callcl(#fname(F),Es) ... </k>











rule <k> #Index(E1:ExpCL,E2:ExpCL) => #read(#PArray(#replaceVar(E1,ENV),E2)) ... </k>
     <env> ENV:Map </env>


rule boolval(false) => (1)
rule boolval(true)  => (0)






rule <k> #ExpCLs(V:ValueCL, VL:ValueCLs) => #ValCLs(V, VL) ... </k>







rule <k> E1:ExpCL :=cl E:ExpCL
     	 => #writecl(#replaceVar(E1,ENV), E) ... </k>
     <env> ENV:Map </env>


rule <k> E1:ExpCL :=cl #uninit(0)
     	 => #writecl(#replaceVar(E1,ENV), #uninit(0)) ... </k>
     <env> ENV:Map </env>




rule <k> #writecl(P:Path, V:ValueCL) => #write(P,V) ... </k>








rule <k> refcl(I:Int,E:ExpCL) => #Brw(I,#replaceVar(E,ENV)) ... </k>
     <env> ENV:Map </env>

rule <k> assertcl((0)) => #unit ... </k>


syntax ExpCL
       ::= #Move(KItem,KItem)		[strict(1)]

rule <k> #move(E:ExpCL) =>
     	 #Move(#read(#replaceVar(E,ENV)),
	 #write(#replaceVar(E,ENV),#uninit(0))) ... </k>
     <env> ENV:Map </env>
     requires ((termIndex(E) ==Int 10000) orBool (termIndex(E) ==Int 100021) orBool
              (termIndex(E) ==Int 100022) orBool (termIndex(E) ==Int 100054))

rule <k> #move(E) => E ... </k>
     requires (notBool (
     	      (termIndex(E) ==Int 10000) orBool (termIndex(E) ==Int 100021) orBool
              (termIndex(E) ==Int 100022)
     ))


rule <k> #Move(V:ValueCL, WT:KItem) => WT ~> V ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
              andBool termIndex(V) ==Int 100016

rule <k> #Move(V:ValueCL, WT:KItem) => V ... </k>
     requires andBool termIndex(V) =/=Int 100016




rule #printStr(S:String,#ValCLs(V:ValueCL, Vs:ValueCLs))
            => #printStr(replaceFirst(S,"{}", #valtoStr(V)), Vs) 

rule #printStr(S:String, .ValueCLs) => S


syntax String
       ::= #valtoStr(ValueCL)                   [function]
         | #printStr(String,ValueCLs)             [function]

rule #valtoStr((I:Int)) => Int2String(I)
rule #valtoStr((S:String)) => S

rule <k> clskip => #unit ... </k>



syntax ExpCL
       ::= #forkclInit(KItem,KItem,KItem)   [strict(2,3)]

rule <k> #forkcl((S:String), #Mut(L:Path)) => #forkclInit((S), 
        #read(L . (1)), #read(L . (0)))  ... </k>



rule <k> #forkclInit((S:String), cr(NTH:Int), V:ValueCL) =>  #threadId( N ) ... </k>
     <activeThreads> .Set => SetItem( N ) ... </activeThreads>
     <cntThreads> N:Int => (N +Int 1) </cntThreads>
     (.Bag =>
      <Thread>
          <threadId> N  </threadId>
	  <tname> S </tname>
	  <k> 
	      callcl(cr(NTH), V) ~> #endThread(N)
	  </k>
	  <env> .Map </env>
	  <clstack> .List </clstack>
      </Thread>
     )


rule
      ((<Thread>
         ...
	 <threadId> TID:Int </threadId>
         <k> #destructThread </k>
         <env> .Map </env>
         <clstack> .List </clstack>
         ...
         </Thread>
         ) => .Bag)

rule    (<Thread>
         ...
	 <threadId> 0 </threadId>
         <k> #unit </k>
         <env> .Map </env>
         <clstack> .List </clstack>
         ...
         </Thread>
         )

rule
      (<Thread>
         ...
	 <threadId> 0:Int </threadId>
         <k> .K </k>
         <env> .Map </env>
         <clstack> .List </clstack>
         ...
         </Thread>
         ) 


rule <k> #endThread(I:Int) => #destructThread </k>
     <activeThreads> ... SetItem(I) => .Set ... </activeThreads>


rule <k> #wait(#threadId(I:Int)) => #waitcl(#threadId(I)) ... </k>
     
syntax ExpCL
       ::= #waitcl(ExpCL)

rule <threadId> TID:Int </threadId>
     <k> #waitcl(#threadId(I:Int)) => #unit ... </k>
     <activeThreads> SS:Set </activeThreads>
     requires (notBool #inSet(I,SS))



rule <k> #loop C:ExpCL {E:ExpCL} => case C of {E ; #loopstart(#loop C {E}), #unit} ... </k>


rule <k> #break ~> #loop C:ExpCL {E:ExpCL} => #break ... </k>

rule <k> #break ~> #loopstart(#loop C:ExpCL {E:ExpCL}) => #unit ... </k>

rule <k> #loopstart(E:ExpCL) => E ... </k>


rule <k> #break ~> let X:Ident = E:ExpCL => #break ... </k>

rule <k> #break ~> E1:ExpCL ; E2:ExpCL => #break ~> E1 ~> E2 ... </k>

rule <k> #break ~> X:Ident => #break ... </k>

rule <k> #break ~> V:ValueCL => #break ... </k>

rule <k> #break ~> #cvarcl(E:ExpCL) => #break ... </k>

rule <k> #break ~> M:MemOps => #break ... </k>

rule <k> #break ~> * E:ExpCL => #break ... </k>

rule <k> #break ~> E:ExpCL . E1:ExpCL => #break ... </k>

rule <k> #break ~> refcl(_,E:ExpCL) => #break ... </k>

rule <k> #break ~> #refallocate(E:ExpCL) => #break ... </k>

rule <k> #break ~> callcl(E:ExpCL,Es:ExpCLs) => #break ... </k>

rule <k> #break ~> pcallcl(E1:ExpCL,E2:ExpCL,Es:ExpCLs) => #break ... </k>

rule <k> #break ~> A:ArithExpCL => #break ... </k>

rule <k> #break ~> E:ExpCL :=cl E1:ExpCL => #break ... </k>

rule <k> #break ~> E:ExpCL :=cl E1:ExpCL => #break ... </k>

rule <k> #break ~> F:FnCL => #break ... </k>

rule <k> #break ~> (| E:ExpCL |) => #break ... </k>

rule <k> #break ~> assertcl(E:ExpCL) => #break ... </k>

rule <k> #break ~> clskip => #break ... </k>

rule <k> #break ~> #move(E:ExpCL) => #break ... </k>

rule <k> #break ~> #moveVal(E1:ExpCL, E2:ExpCL) => #break ... </k>

rule <k> #break ~> S:StrExp => #break ... </k>

rule <k> #break ~> #paniccl(E:ExpCL) => #break ... </k>

rule <k> #break ~> #rawcl(E:ExpCL) => #break ... </k>

rule <k> #break ~> #print(S:String,E:ExpCLs) => #break ... </k>

rule <k> #break ~> #retcl(E2:ExpCL) => #break ... </k>

rule <k> #break ~> #break => #break ... </k>


rule <k> #refallocate(#own(B)) => #Shr(#sl(SA)) ... </k>
     <sloc> SLOC:Map => SLOC[SA<- #own(B)] </sloc>
     <slocAddr> SA:Int => SA +Int 1 </slocAddr>


rule <k> pcallcl(E,E1,Es) => #pCallcl(E,E1,Es) ... </k>

rule <k> #pCallcl((_:Int), F:Ident, Es:ExpCLs) =>  callcl(#cpath(i32,F), Es) ... </k>

rule <k> #pCallcl(#own(#blk(B:Int)), F:Ident, Es:ExpCLs) => callcl(#cpath(#owner(T),F), Es) ... </k>
     <blocks> ... B |-> #blkInfo(_,T) ... </blocks>


syntax ExpCL
       ::= #pCallcl(KItem,ExpCL,ExpCLs)		[strict(1)]

rule <k> #pCallcl(#Shr(P:Path), F:Ident, ES:ExpCLs) => #pCallcl(#read(P),F,ES) ... </k>

rule <k> #pCallcl(#Mut(P:Path), F:Ident, ES:ExpCLs) => #pCallcl(#read(P),F,ES) ... </k>




syntax KCL
       ::= #deAllocate(KItem)		[strict]

rule <k> #deallocate(E:ExpCL) => #deAllocate(#lv(#replaceVar(E,ENV))) ... </k>
     <env> ENV:Map </env>


rule <k> #deAllocate(#pair(#sl(I:Int),(true))) => #free(#blk(B)) ... </k>
     <sloc> ... I |-> #Raw(#heap(B:Int,0)) ... </sloc>


rule <k> #deAllocate(#pair(#heap(B1,O),(true))) => #free(#blk(B)) ... </k>
     <blocks> ... #heap(B1,O) |-> #Raw(#heap(B:Int,0)) ... </blocks>

rule <k> #deAllocate(#pair(#Raw(#heap(B1,O)),(true))) => #free(#blk(B)) ... </k>
     <blocks> ... #heap(B1,O) |-> #Raw(#heap(B:Int,0)) ... </blocks>


rule <k> #AllocateInit((N:Int),T:CoreTy, Vs:ValueCLs) => #allocateInit((N),T,Vs) ... </k>

rule <k> #AllocateInitRaw((N:Int),T:CoreTy, Vs:ValueCLs) => #allocateInitRaw((N),T,Vs) ... </k>

rule <k> #Allocate((N:Int),T:CoreTy) => #allocate((N),T) ... </k>


rule <k> #RawCL(#Shr(#sl(L))) => #Raw(#sl(L)) ... </k>

rule <k> #RawCL(#Mut(#sl(L))) => #Raw(#sl(L)) ... </k>

rule <k> #RawCL(#own(#blk(B:Int))) => #Raw(#heap(0,0)) ... </k>


syntax KItem
       ::= #ret(KItem)		[strict]

rule <k> #retcl(E:ExpCL) => #ret(E) ... </k>


rule <k> #ret(V:ValueCL) =>  #ret(V) ... </k>
     <clstack> ListItem(Rho:Map) => .List ... </clstack>


rule <k> #ret(V:ValueCL) ~> _:K => #computeFB(V) </k>
     <env> _ => ENV </env>
     <clstack> ListItem(#state(RestPGM:K,ENV:Map)) ... </clstack>


rule <k> #ret(V:ValueCL) => #free(V1) ~> #ret(V) ... </k>
     <env> ... ((X |-> #sl(L:Int)) => .Map) ...  </env>
     <clstack> ListItem(X:Ident) => .List ... </clstack>
     <sloc> ... ((L |-> #own(V1)) => .Map) ... </sloc>


rule <k> #ret(V:ValueCL) => #ret(V) ... </k>
     <env> ... ((X |-> #sl(L:Int)) => .Map) ...  </env>
     <clstack> ListItem(X:Ident) => .List ... </clstack>
     <sloc> ... ((L |-> V1) => .Map) ... </sloc>
     requires(termIndex(V1) =/=Int 100016)


rule #list2Set(.List) => .Set
rule #list2Set(ListItem(E) L:List) => SetItem(E) #list2Set(L)

rule #unit ~> #ret(V:ValueCL) => #ret(V)


syntax ExpCL
       ::= #Memcopy(KItem,KItem,ExpCL)  [seqstrict(1,2)]

rule <k> #memcopy(E1:ExpCL, E2:ExpCL, (N:Int))
             => #Memcopy(E1, E2, (N)) ...  </k>
     <env> ENV:Map </env>
     
syntax ExpCL
       ::=#memcpyI(KItem,KItem)	[strict(2)]



rule <k> #Memcopy(#Raw(#heap(B1:Int,I1:Int)), #Raw(#heap(B2:Int,I2:Int)), (N:Int))
     => #memcpyI(#heap(B2,I2),#readna(#heap(B1,I1)))
                 ~> #Memcopy(#Raw(#heap(B1,I1+Int 1)),
		            #Raw(#heap(B2, I2+Int 1)), (N-Int 1)) ... </k>
     requires (N >Int 0)

rule #Memcopy(_,_,(N:Int)) => #unit
     requires N <=Int 0

rule #memcpyI(#heap(B1:Int,I1:Int),V:ValueCL) => #writena(#heap(B1,I1),V)

syntax ExpCL
       ::= #Reallocate(KItem,Int,Int)

rule <k> #reallocate(#Raw(#heap(B:Int,O:Int)), (N:Int)) => 
			   #Reallocate(#heap(B,O), N1, N) ... </k>
     <blocks> ... B |-> #blkInfo((N1:Int => N),_)   ... </blocks>

syntax Map
       ::= #CStoreI(Int,Int,Int)		[function]

rule #CStoreI(B:Int,I:Int,N:Int) => (#heap(B,I) |-> #uninit(0)) #CStoreI(B,I +Int 1, N)
     requires I <Int N
     
rule #CStoreI(B:Int,N:Int,N:Int) => .Map


rule <k> #Reallocate(#heap(B:Int,O:Int), N1:Int,N2:Int) => #unit  ... </k>
     <blocks> .Map => #heap(B,N1) |-> #uninit(0) ... </blocks>
     <memStatus> .Map => #heap(B,N1) |-> #ms(0,0) ...  </memStatus>

rule <k> #Reallocate(#heap(B:Int,O:Int), N1:Int,N2:Int) => #unit  ... </k>


rule #offset(#Raw(#heap(B:Int,O:Int)), (L:Int)) => #Raw(#heap(B,O +Int L))

*/

endmodule


