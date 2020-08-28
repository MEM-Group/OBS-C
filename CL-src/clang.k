
require "clang-syntax.k"
require "memory.k"
require "sortindexcl.k"
require "vec-pattern.k"



module CLANG

imports CLANG-SYNTAX
imports CONFIGURATION
imports MEMORY-SYNTAX
imports MEMORY
imports SORTINDEXCL
imports DOMAINS
imports FLOAT
imports COLLECTIONS

syntax KItem
       ::= #computeFB(KItem)			[strict]
         | #state(K, Map)
	 | fnCalls(Int, Idents, ValueCLs)
	 | InitParam(Ident,ValueCL)

rule <k> let X:Ident = V:ValueCL => #unit ... </k>
     <env> ENV:Map => ENV[X <- #sl(SA)]   </env>
     <clstack> .List => (ListItem(X) ListItem(ENV)) ... </clstack>
     <sloc> SLOC:Map => SLOC[SA <- #clkVal(V,CLK)]  </sloc>
     <clk> CLK:Int => CLK +Int 1 </clk>
     <slocAddr> SA:Int => SA +Int 1 </slocAddr>
     <memStatus> .Map => #sl(SA) |-> #ms(0,0) ...  </memStatus>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> let X:Ident = V:ErrorState => #unit ... </k>
     <env> ENV:Map => ENV[X <- #sl(SA)]   </env>
     <clstack> .List => (ListItem(X) ListItem(ENV)) ... </clstack>
     <sloc> SLOC:Map => SLOC[SA <- V]  </sloc>
     <clk> CLK:Int => CLK +Int 1 </clk>
     <slocAddr> SA:Int => SA +Int 1 </slocAddr>
     <memStatus> .Map => #sl(SA) |-> #ms(0,0) ...  </memStatus>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #computeFB(V:ValueCL) => V ~> RestPGM ... </k>
     <env> _ => Rho </env>
     <clstack> ListItem(#state(RestPGM, Rho:Map)) => .List ... </clstack>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #computeFB(V:ValueCL) => V ... </k>
     <env> _ => Rho </env>
     <clstack> ListItem(Rho:Map) => .List ... </clstack>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #unit ~> #computeFB(V) => #computeFB(V) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> (| E:ExpCL |) => E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #computeFB(V:ValueCL) =>  #free(V1) ~> #computeFB(V) ... </k> // #free(V1) ~>
     <env> ... ((X |-> #sl(L)) => .Map) ...  </env>
     <clstack> ListItem(X:Ident) => .List ... </clstack>
     <sloc> ... ((L |-> #own(V1)) => .Map) ... </sloc>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #computeFB(V:ValueCL) => #computeFB(V) ... </k>
     <env> ... ((X |-> #sl(L)) => .Map) ...  </env>
     <clstack> ListItem(X:Ident) => .List ... </clstack>
     <sloc> ... ((L |-> V1) => .Map) ... </sloc>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1) andBool (termIndex(V1) =/=Int 100016)


syntax K
       ::= #DESTRUCT(ValueCL)
         | #destruct(Bool,K)

rule <k> #DESTRUCT(V:ValueCL) => #destruct(termIndex(V) ==Int 100016, V) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #destruct(true,#own(D)) => #free(D) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #destruct(false,_) => . ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> E1:ExpCL . E2:ExpCL  => #read(#replaceVar(E1 . E2, ENV)) ... </k> 
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> * E:ExpCL => #read(#replaceVar(* E, ENV)) ... </k> 
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

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
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

 

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
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <Thread>
      <threadId> TID:Int </threadId>
      <k> E1:ExpCL ; E2:ExpCL => E1 ~> E2 ... </k>
      ...
     </Thread>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> V:ValueCL ~> E2:ExpCL => #selectE(V =/=K #break, E2, .K) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #fname(F:Ident) => cr(I) ... </k>
     <funclosure>  ... (F |-> I:Int) ...  </funclosure>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #fname(F:Ident) => F ... </k>
     <funclosure> Rho:Map  </funclosure>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1) andBool (notBool (#inSet(F, keys(Rho))))

rule <k> #fname(cr(I:Int)) => cr(I) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #fname(E1:ExpCL . E2:ExpCL) => E1 . E2 ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #fname(* E:ExpCL) => * E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule #inSet(E:KItem, SetItem(E) Rest:Set) => true

rule #inSet(E:K, .Set) => false
rule #inSet(E:K, SetItem(E1) Rest:Set) => #inSet(E,Rest)
     requires E =/=K E1




syntax FName
       ::= #fname(ExpCL)

syntax ExpCL
       ::= #callcl(KItem, ExpCLs)		[strict]

rule <k> callcl(F:ExpCL,Es:ExpCLs) => #callcl(#fname(F),Es) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #callcl(cr(I:Int), VL:ValueCLs) ~> RestPGM:K => fnCalls(I,P,VL) </k>
     <crId> I </crId>
     <crParams> P:Idents </crParams>
     <env> Rho:Map </env>
     <clstack> .List => (ListItem(#state(RestPGM, Rho))) ... </clstack>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> fnCalls(I:Int, (X:Ident, RestP:Idents), V:ValueCL, RestVl:ValueCLs) =>
         InitParam(X,V) ~> fnCalls(I,RestP,RestVl) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> InitParam(X:Ident, V:ValueCL) => . ... </k>
     <env> ENV:Map => ENV[X <- #sl(SA)] </env>
     <clstack> .List => ListItem(X) ... </clstack>
     <sloc> SLOC:Map => SLOC[SA <- #clkVal(V,CLK)] </sloc>
     <memStatus> .Map => #sl(SA) |-> #ms(0,0) ...  </memStatus>
     <clk> CLK:Int => CLK +Int 1 </clk>
     <slocAddr> SA:Int => SA +Int 1 </slocAddr>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> fnCalls(I:Int, .Idents, .ValueCLs) => #computeFB(B) ... </k>
     <crId> I </crId>
     <crBody> B:ExpCL </crBody>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



syntax CaseItem
       ::= "#caseItem2" "(" ExpCL ";" ExpCL "," GuardedActions ")"              [strict(1)]
         | caseItem(Int,ExpCLs)

rule <k> case (I) of { EL:ExpCLs}  => caseItem(boolval(I), EL) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> caseItem(I:Int,_:ExpCL,EL:ExpCLs)
     	    => caseItem(I-Int 1, EL) ...  </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1) andBool (I >Int 0 )

rule <k> caseItem(0,E:ExpCL,EL:ExpCLs) => E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> caseItem(0,#ValCLs(E:ValueCL, EL:ValueCLs)) => E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> case { G:ExpCL -> E:ExpCL ; GAs:GuardedActions } => #caseItem2(G;E,GAs) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #caseItem2((0); E, GAs:GuardedActions) => E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #caseItem2((1); E, GAs:GuardedActions) => case {GAs} ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #caseItem2((true); E, GAs:GuardedActions) => E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #caseItem2((false); E, GAs:GuardedActions) => case {GAs} ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> if E:ExpCL then E1:ExpCL else E2:ExpCL 
     	  	  	 => case E of {E2, E1} ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Int) ^ (Y:Int) => (X ^Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> (X:Int) - (Y:Int) => (X -Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Int) + (Y:Int) => (X +Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> (X:Float) + (Y:Float) => (X +Float Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

                                                                                            

rule <k> (X:Int) & (Y:Int) => (X &Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Int) ** (Y:Int) => (X *Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Float) ** (Y:Float) => (X *Float Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Float) / (Y:Float) => (X /Float Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> (X:Float) - (Y:Float) => (X -Float Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Int) / (Y:Int) => (X /Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




rule <k> (X:Int) <= (Y:Int) => (X <=Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Int) < (Y:Int) => (X <Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Int) == (Y:Int) => (X ==Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> (X:String) == (Y:String) => (X ==String Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> (X:Int) > (Y:Int) => (X >Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> (X:Int) >= (Y:Int) => (X >=Int Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




rule <k> (X:Int) mod (Y:Int) => (X modInt Y) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> (X:Int) && (Y:Int) => (#int2bool(X) andBool #int2bool(Y)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> (X:Int) || (Y:Int) => (#int2bool(X) orBool #int2bool(Y)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax Bool
       ::= #int2bool(Int)                       [function]

rule #int2bool(0) => true
rule #int2bool(1) => false



rule <k> X:Ident => #read(L) ... </k>
     <env> ... X |-> L ... </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #Index(E1:ExpCL,E2:ExpCL) => #read(#PArray(#replaceVar(E1,ENV),E2)) ... </k>
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule boolval(false) => (1)
rule boolval(true)  => (0)



rule <k> .ExpCLs => .ValueCLs ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ExpCLs(V:ValueCL, VL:ValueCLs) => #ValCLs(V, VL) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



syntax Path
       ::= #replaceVar(ExpCL,Map)		[function]





rule <k> E1:ExpCL :=cl E:ExpCL
     	 => #writecl(#replaceVar(E1,ENV), E) ... </k>
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> E1:ExpCL :=cl #uninit(0)
     	 => #writecl(#replaceVar(E1,ENV), #uninit(0)) ... </k>
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




rule <k> #writecl(P:Path, V:ValueCL) => #write(P,V) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule #replaceVar(X:Ident, M:Map) => #etrPath(M[X])
rule #replaceVar(E1:ExpCL . E2:ExpCL, M:Map) => #PFieldExpCL(#replaceVar(E1,M), E2)
rule #replaceVar(* E:ExpCL, M:Map) => #dRef(#replaceVar(E,M))
rule #replaceVar(#Index(E1:ExpCL,E2:ExpCL),M:Map) => #PArray(#replaceVar(E1,M),E2)




syntax Path
       ::= #etrPath(K)		[function]

rule #etrPath(#sl(L:Int)) => #sl(L)
rule #etrPath(#PFieldExpCL(P, I)) => #PFieldExpCL(P,I)


rule <k> refcl(I:Int,E:ExpCL) => #Brw(I,#replaceVar(E,ENV)) ... </k>
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> assertcl((0)) => #unit ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax ExpCL
       ::= #Move(KItem,KItem)		[strict(1)]

rule <k> #move(E:ExpCL) =>
     	 #Move(#read(#replaceVar(E,ENV)),
	 #write(#replaceVar(E,ENV),#uninit(0))) ... </k>
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
      andBool ((termIndex(E) ==Int 10000) orBool (termIndex(E) ==Int 100021) orBool
              (termIndex(E) ==Int 100022) orBool (termIndex(E) ==Int 100054))

rule <k> #move(E) => E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool (notBool (
     	      (termIndex(E) ==Int 10000) orBool (termIndex(E) ==Int 100021) orBool
              (termIndex(E) ==Int 100022)
     ))


rule <k> #Move(V:ValueCL, WT:KItem) => WT ~> V ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
              andBool termIndex(V) ==Int 100016

rule <k> #Move(V:ValueCL, WT:KItem) => V ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
     andBool termIndex(V) =/=Int 100016


rule <k> #print(S:String, Vs:ValueCLs) => #unit ... </k>
     <output> .List => ListItem(#printStr(S,Vs)) ... </output>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule #printStr(S:String,#ValCLs(V:ValueCL, Vs:ValueCLs))
            => #printStr(replaceFirst(S,"{}", #valtoStr(V)), Vs) 

rule #printStr(S:String, .ValueCLs) => S


syntax String
       ::= #valtoStr(ValueCL)                   [function]
         | #printStr(String,ValueCLs)             [function]

rule #valtoStr((I:Int)) => Int2String(I)
rule #valtoStr((S:String)) => S

rule <k> clskip => #unit ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



syntax ExpCL
       ::= #forkclInit(KItem,KItem,KItem)   [strict(2,3)]

rule <k> #forkcl((S:String), #Mut(L:Path)) => #forkclInit((S), 
        #read(L . (1)), #read(L . (0)))  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



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
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




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
     <cthread> CTID:Int => -1  </cthread>
     requires (CTID ==Int TID) orBool (CTID ==Int -1)

rule
      (<Thread>
         ...
	 <threadId> 0 </threadId>
         <k> #unit </k>
         <env> .Map </env>
         <clstack> .List </clstack>
         ...
         </Thread>
         )
     <cthread> CTID:Int => -1  </cthread>
     requires CTID ==Int 0 

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
     <cthread> CTID:Int => -1  </cthread>
     requires (CTID ==Int 0)





rule <k> #endThread(I:Int) => #destructThread </k>
     <activeThreads> ... SetItem(I) => .Set ... </activeThreads>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #wait(#threadId(I:Int)) => #waitcl(#threadId(I)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => -1  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1
     
syntax ExpCL
       ::= #waitcl(ExpCL)

rule <threadId> TID:Int </threadId>
     <k> #waitcl(#threadId(I:Int)) => #unit ... </k>
     <activeThreads> SS:Set </activeThreads>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires   (TID ==Int CTID orBool CTID ==Int -1)
        andBool (notBool #inSet(I,SS))



rule <k> #loop C:ExpCL {E:ExpCL} => case C of {E ; #loopstart(#loop C {E}), #unit} ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #break ~> #loop C:ExpCL {E:ExpCL} => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #loopstart(#loop C:ExpCL {E:ExpCL}) => #unit ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1





rule <k> #loopstart(E:ExpCL) => E ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #break ~> let X:Ident = E:ExpCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> E1:ExpCL ; E2:ExpCL => #break ~> E1 ~> E2 ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> X:Ident => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> V:ValueCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #cvarcl(E:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> M:MemOps => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> * E:ExpCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> E:ExpCL . E1:ExpCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> refcl(_,E:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #refallocate(E:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> callcl(E:ExpCL,Es:ExpCLs) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> pcallcl(E1:ExpCL,E2:ExpCL,Es:ExpCLs) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> A:ArithExpCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> E:ExpCL :=cl E1:ExpCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> E:ExpCL :=cl E1:ExpCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> F:FnCL => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> (| E:ExpCL |) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> assertcl(E:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> clskip => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #move(E:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #moveVal(E1:ExpCL, E2:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> S:StrExp => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #paniccl(E:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #rawcl(E:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #print(S:String,E:ExpCLs) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #break ~> #retcl(E2:ExpCL) => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #break ~> #break => #break ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #refallocate(#own(B)) => #Shr(#sl(SA)) ... </k>
     <sloc> SLOC:Map => SLOC[SA<- #own(B)] </sloc>
     <slocAddr> SA:Int => SA +Int 1 </slocAddr>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> pcallcl(E,E1,Es) => #pCallcl(E,E1,Es) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #pCallcl((_:Int), F:Ident, Es:ExpCLs) =>  callcl(#cpath(i32,F), Es) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #pCallcl(#own(#blk(B:Int)), F:Ident, Es:ExpCLs) => callcl(#cpath(#owner(T),F), Es) ... </k>
     <blocks> ... B |-> #blkInfo(_,T) ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax ExpCL
       ::= #pCallcl(KItem,ExpCL,ExpCLs)		[strict(1)]

rule <k> #pCallcl(#Shr(P:Path), F:Ident, ES:ExpCLs) => #pCallcl(#read(P),F,ES) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #pCallcl(#Mut(P:Path), F:Ident, ES:ExpCLs) => #pCallcl(#read(P),F,ES) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1




syntax KCL
       ::= #deAllocate(KItem)		[strict]

rule <k> #deallocate(E:ExpCL) => #deAllocate(#lv(#replaceVar(E,ENV))) ... </k>
     <env> ENV:Map </env>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #deAllocate(#pair(#sl(I:Int),(true))) => #free(#blk(B)) ... </k>
     <sloc> ... I |-> #Raw(#heap(B:Int,0)) ... </sloc>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #deAllocate(#pair(#heap(B1,O),(true))) => #free(#blk(B)) ... </k>
     <blocks> ... #heap(B1,O) |-> #Raw(#heap(B:Int,0)) ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #deAllocate(#pair(#Raw(#heap(B1,O)),(true))) => #free(#blk(B)) ... </k>
     <blocks> ... #heap(B1,O) |-> #Raw(#heap(B:Int,0)) ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #AllocateInit((N:Int),T:CoreTy, Vs:ValueCLs) => #allocateInit((N),T,Vs) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #AllocateInitRaw((N:Int),T:CoreTy, Vs:ValueCLs) => #allocateInitRaw((N),T,Vs) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #Allocate((N:Int),T:CoreTy) => #allocate((N),T) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1



rule <k> #RawCL(#Shr(#sl(L))) => #Raw(#sl(L)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #RawCL(#Mut(#sl(L))) => #Raw(#sl(L)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

rule <k> #RawCL(#own(#blk(B:Int))) => #Raw(#heap(0,0)) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


syntax KItem
       ::= #ret(KItem)		[strict]

rule <k> #retcl(E:ExpCL) => #ret(E) ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ret(V:ValueCL) =>  #ret(V) ... </k>
     <clstack> ListItem(Rho:Map) => .List ... </clstack>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ret(V:ValueCL) ~> _:K => #computeFB(V) </k>
     <env> _ => ENV </env>
     <clstack> ListItem(#state(RestPGM:K,ENV:Map)) ... </clstack>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ret(V:ValueCL) => #free(V1) ~> #ret(V) ... </k>
     <env> ... ((X |-> #sl(L:Int)) => .Map) ...  </env>
     <clstack> ListItem(X:Ident) => .List ... </clstack>
     <sloc> ... ((L |-> #own(V1)) => .Map) ... </sloc>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1


rule <k> #ret(V:ValueCL) => #ret(V) ... </k>
     <env> ... ((X |-> #sl(L:Int)) => .Map) ...  </env>
     <clstack> ListItem(X:Ident) => .List ... </clstack>
     <sloc> ... ((L |-> V1) => .Map) ... </sloc>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)
              andBool (termIndex(V1) =/=Int 100016)


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
    <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1)    
         andBool (N >Int 0)

rule #Memcopy(_,_,(N:Int)) => #unit
     requires N <=Int 0

rule #memcpyI(#heap(B1:Int,I1:Int),V:ValueCL) => #writena(#heap(B1,I1),V)

syntax ExpCL
       ::= #Reallocate(KItem,Int,Int)

rule <k> #reallocate(#Raw(#heap(B:Int,O:Int)), (N:Int)) => 
			   #Reallocate(#heap(B,O), N1, N) ... </k>
     <blocks> ... B |-> #blkInfo((N1:Int => N),_)   ... </blocks>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires TID ==Int CTID orBool CTID ==Int -1

syntax Map
       ::= #CStoreI(Int,Int,Int)		[function]

rule #CStoreI(B:Int,I:Int,N:Int) => (#heap(B,I) |-> #uninit(0)) #CStoreI(B,I +Int 1, N)
     requires I <Int N
     
rule #CStoreI(B:Int,N:Int,N:Int) => .Map


rule <k> #Reallocate(#heap(B:Int,O:Int), N1:Int,N2:Int) => #unit  ... </k>
     <blocks> .Map => #heap(B,N1) |-> #uninit(0) ... </blocks>
     <memStatus> .Map => #heap(B,N1) |-> #ms(0,0) ...  </memStatus>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1) andBool (N1 <Int N2)

rule <k> #Reallocate(#heap(B:Int,O:Int), N1:Int,N2:Int) => #unit  ... </k>
     <threadId> TID:Int </threadId>
     <cthread> CTID:Int => #cThread(CTID,TID)  </cthread>
     requires (TID ==Int CTID orBool CTID ==Int -1) andBool (N1 >=Int N2)


rule #offset(#Raw(#heap(B:Int,O:Int)), (L:Int)) => #Raw(#heap(B,O +Int L))


endmodule


