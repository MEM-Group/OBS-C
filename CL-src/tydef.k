

module TYDEF

imports DOMAINS-SYNTAX
imports DOMAINS


syntax ScalarTy
       ::= "i32"
         | "bool"
	 | "str"
	 | "f32"

syntax PointTy
       ::= #owner(CoreTy)			
       	 | #ref(CoreTy)			
	 | #raw(CoreTy)			

syntax CompTy
       ::= #sum(Fields)			
	 | #prod(Fields)
	    
syntax Field
       ::= #fd(Ident, CoreTy)		

syntax Fields
       ::= List{Field, ","}		[klabel(#Fields)]

syntax FnTy
       ::= "fnTy" "(" CoreTys ")" "->" CoreTy 


syntax TyVar
       ::= #ty(Ident)

syntax CoreTy
       ::= ScalarTy
         | PointTy
	 | CompTy
	 | FnTy
	 | TyVar
	 | "#void" 		//void type
	 | "#TyUndef"
	 | #array(CoreTy,Int)
	 | #trait(Ident)
	 | #IdTy(Ident, CoreTy)
	 | #closureTy(Ident, CoreTy)
	 | "#tyself"
	 | #getTyAt(Fields,Int)			[function]
	 | #getTyof(Fields,Ident)		[function]
	 | #vec(CoreTy)

syntax Int
       ::= #getIndexof(Fields,Ident)		[function]
         | #inFields(Fields,Ident)		[function]



syntax CoreTys
       ::= List{CoreTy, ","}			[klabel(#CoreTys)]


syntax Lifetime
       ::= "static"
       	 | "'" Ident			[klabel(#lftVar)] 
         | "'" "static"			[klabel(#lftStatic)] 


syntax Mut
       ::= "mut"
         | "imm"


syntax Ident




/*@***************************************************************

		Rust Type Definition

******************************************************************/

syntax RSType
       ::= CoreTy

syntax RSType
       ::= "usize"
         | "isize"
         | Ident				
	 ////////////////////
         | "*" "const" RSType			[klabel(#RawConst)]		
         | "*" "mut"   RSType			[klabel(#RawMut)]		
	 ////////////////////
	 | "&" RSType				[klabel(#RefTy)]
	 | "&" Lifetime RSType			[klabel(#RefTyL)]
	 | "&" "mut" RSType			[klabel(#MutRefTy)]
	 | "&" Lifetime "mut" RSType		[klabel(#MutRefTyL)]
	 ////////////////////
	 | Ident "<" TParams ">"		[klabel(#paramTys)]
	 | "(" RSType ")"			[bracket]
//	 | "Vec" "<" RSType ">"			[klabel(#VecRs)]

//syntax Id
//       ::= "Vec"   [reject]


syntax TParam
       ::= TyVar
         | Ident
       	 | Lifetime
	 | RSType

syntax TParams
       ::= List{TParam, ","}			[klabel(#TParams)]



syntax RSTypes
       ::= List{RSType, ","}			[klabel(#RSTypes)]
         | CoreTys


syntax CoreTy
       ::= #RStoCT(K,Map)		[function]


syntax CoreTys
       ::= #RStoCTs(RSTypes,Map)		[function]

rule #RStoCT(& mut T:RSType, M:Map) => #ref( #RStoCT(T,M) )

rule #RStoCT(& T:RSType, M:Map)      => #ref( #RStoCT(T,M) )

rule #RStoCT(* const T:RSType, M:Map) => #raw(#RStoCT(T,M))
rule #RStoCT(* mut T:RSType, M:Map) =>   #raw(#RStoCT(T,M))

rule #RStoCT([T:RSType; N:Int], M:Map) => #owner(#array(#RStoCT(T,M),N))

rule #RStoCT(T:CoreTy, M:Map) => T
     requires T =/=K str

rule #RStoCT(str, M:Map) => #owner(str)
     

//This rule should be modified in the future

rule #RStoCT(usize, M:Map) => i32
rule #RStoCT(isize, M:Map) => i32


//rule #RStoCT(Vec < T:RSType >, M:Map) =>  #owner(#vec(#RStoCT(T,M)))

rule #RStoCT(& L:Lifetime T:RSType, M:Map) => #RStoCT(& T, M)
rule #RStoCT(& L:Lifetime mut T:RSType, M:Map) => #RStoCT(& T, M)

rule #RStoCT(& T:RSType, M:Map) => #ref(#RStoCT(T,M))
rule #RStoCT(& mut T:RSType, M:Map) => #ref(#RStoCT(T,M))

rule #RStoCT(* const T:Ident, M:Map) => #raw(#RStoCT(T,M))
rule #RStoCT(* mut T:Ident,M:Map) => #raw(#RStoCT(T,M))


rule #RStoCT(X:Ident < _:TParams  >, M) => #RStoCT(X, M)

rule #RStoCT(X:Ident, M:Map) => #ty(X)
     requires notBool (X in keys(M))




/********************************************
	auxiliary function for types
*********************************************/

rule #getTyAt(#fd(_,T:CoreTy),Fs:Fields, 0:Int) => T
rule #getTyAt(F:Field,Fs:Fields, I:Int) => #getTyAt(Fs,I -Int 1)
     requires I >Int 0



rule #getTyof(#fd(N:Ident,T:CoreTy),Fs:Fields, N) => T
rule #getTyof(#fd(N1:Ident,T:CoreTy),Fs:Fields, N:Ident) => #getTyof(Fs,N)
     requires N1 =/=K N

rule #getIndexof(#fd(N:Ident,T:CoreTy),  Fs:Fields, N) => 0

rule #getIndexof(#fd(N1:Ident,T:CoreTy), Fs:Fields, N:Ident) => 1 +Int #getIndexof(Fs,N)
     requires N1 =/=K N


rule #inFields(#fd(N:Ident,T:CoreTy), Fs:Fields, N) => 1

rule #inFields(#fd(N:Ident,T:CoreTy), Fs:Fields, N1:Ident) => #inFields(Fs,N1)
     requires N1 =/=K N

rule #inFields(.Fields, N:Ident) => 0



rule #RStoCTs(T:RSType, Ts:RSTypes, M:Map) => #CoreTys(#RStoCT(T, M), #RStoCTs(Ts, M))

rule #RStoCTs(.RSTypes, _:Map) => .CoreTys

rule #RStoCTs(Cs:CoreTys, M:Map) =>  Cs


syntax CoreTy
	 ::= #unwrapCoreTy(K)				[function]

rule #unwrapCoreTy(T:CoreTy) => T

/******************************************
  Type in Rust some times are generic.
  In order to deal with this condition.
  Each type should have an identifier.
*******************************************/
/*

syntax RType
       ::= #getTyId(RType)				[function]

rule #getTyId(i32) => i32
rule #getTyId(bool) => bool
rule #getTyId(str)  => str
rule #getTyId(#sumId(T:Ident)) => #sumId(T)
rule #getTyId(#prodId(T:Ident)) => #prodId(T)

rule #getTyId(reft(T:RType)) => #getTyId(T)
rule #getTyId(own(T:RType)) => #getTyId(T)
rule #getTyId(tyTrait(T:Ident)) => tyTrait(T)
rule #getTyId([T:RType;I:Int]) => [T;I]
rule #getTyId(raw(T:RType)) => raw(T)
rule #getTyId(tvar(T:Ident)) => tvar(T)

rule #getTyId(#tyself) => #tyself

rule #getTyId(Vec< T:RType >) => Vec< T >
rule #getTyId(Box< T:RType >) => Box< T >
rule #getTyId(Box< T:Ident >) => Box< T >
rule #getTyId(#closureTy(N:K,T:FnTy)) => #closureTy(N,T)
*/

/*****************************************************
		Library of Rust
*****************************************************/

syntax RSType
       ::= "[" RSType ";" Int "]"      [klabel(#rArray)]


syntax CoreTy
       ::= #rArrayId(CoreTy,Int) 	       


syntax Int
       ::= #sizeofCoreTys(CoreTys)		[function]

rule #sizeofCoreTys(.CoreTys) => 0
rule #sizeofCoreTys(T:CoreTy, Ts:CoreTys) => 1 +Int #sizeofCoreTys(Ts)

syntax CoreTy
       ::= #getOwnTy(CoreTy)   [function]

rule #getOwnTy(#ref(T:CoreTy)) => #getOwnTy(T)
rule #getOwnTy(#owner(T:CoreTy)) => #owner(T)
rule #getOwnTy(#raw(T:CoreTy)) => #raw(T)
rule #getOwnTy(#trait(T:Ident)) => #trait(T)
rule #getOwnTy(i32) => i32






endmodule