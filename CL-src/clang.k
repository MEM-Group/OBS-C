
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

rule case B of { EL:ExpCLs}  => #caseItem(B, EL)
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

syntax ExpCL ::= #loop(ExpCL, ExpCL)

rule <k> loop C:ExpCL {E:ExpCL} ~> RestPGM:K => #computeFB( #loop(C,E) ) </k>
     <env> ENV:Map </env>
     <clstack> .List => (ListItem(#state(RestPGM, ENV))) ... </clstack>
     
rule #loop(C,E) => if C then (E; #loop(C,E)) else skip

rule skip => #unit

rule <k> break ~> RestPGMOLD:K => RestPGM </k>
     <env> _ => ENV </env>
     <clstack> ListItem(#state(RestPGM, ENV:Map)) => .List ... </clstack>

rule <k> move(E) => #move(#replaceVar(E,ENV)) ... </k>
     <env> ENV:Map </env>

rule deallocate(#blk(B:Int)) => #free(#blk(B))

/*
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




rule <k> #AllocateInit((N:Int),T:CoreTy, Vs:ValueCLs) => #allocateInit((N),T,Vs) ... </k>

rule <k> #AllocateInitRaw((N:Int),T:CoreTy, Vs:ValueCLs) => #allocateInitRaw((N),T,Vs) ... </k>

rule <k> #Allocate((N:Int),T:CoreTy) => #allocate((N),T) ... </k>
*/

endmodule


