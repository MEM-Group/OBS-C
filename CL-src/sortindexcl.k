
requires "clang-syntax.k"

module SORTINDEXCL

imports CLANG-SYNTAX

syntax Int
       ::= termIndex(K)		[function]

rule termIndex(X:Ident) => 10000
rule termIndex((_:Int)) => 10001
rule termIndex((_:Float)) => 10001
rule termIndex((_:Bool)) => 10002
rule termIndex(#unit) => 10005
rule termIndex(cr(_:Int)) => 10006
rule termIndex(L:Location) => 10007
rule termIndex(#uninit(_:Int)) => 10008
rule termIndex(lhsV(_:Ident)) => 10009
rule termIndex((_:String)) => 100011
rule termIndex(#threadId(_)) => 100012
rule termIndex(#blk(_)) => 100013
rule termIndex(#Shr(_)) => 100014
rule termIndex(#Mut(_)) => 100015

rule termIndex(#shr(_,_,_)) => 100018
rule termIndex(#mut(_,_,_)) => 100019

rule termIndex(#own(_)) => 100016
rule termIndex(#Raw(_)) => 100017

////////////////////////////////////////////

rule termIndex(_:ErrorState) => 100020
rule termIndex(* E:ExpCL) => 100021
rule termIndex(E1:ExpCL . E2:ExpCL) => 100022
rule termIndex(refcl(_,_)) => 100023
rule termIndex(callcl(_,_)) => 100024
rule termIndex(pcallcl(_,_,_)) => 100025
rule termIndex(E:ArithExpCL) => 100026
rule termIndex(E1 :=cl E2) => 100028
rule termIndex(F:FnCL) => 100029
rule termIndex((| E:ExpCL |)) => 100030
rule termIndex(assertcl(_)) => 100031
rule termIndex(clskip) => 100032
rule termIndex(#move(_)) => 100033
rule termIndex(#moveVal(_,_)) => 100034
rule termIndex(_:StrExp) => 100035
rule termIndex(#paniccl(_)) => 100036
rule termIndex(#rawcl(_)) => 100037

//rule termIndex(M:MapOperation) => 100038
rule termIndex(#print(_,_)) => 100039
rule termIndex(#retcl(_)) => 100040
rule termIndex(#endThread(_)) => 100041
rule termIndex(#loop _:ExpCL { _:ExpCL }) => 100042
rule termIndex(#loopstart(_)) => 100043

rule termIndex(#break) => 100044
rule termIndex(#drop(_)) => 100045
rule termIndex(#wait(_)) => 100046
rule termIndex(#deallocate(_)) => 100047
rule termIndex(#destructThread) => 100048
rule termIndex(#memcopy(_,_,_)) => 100049

rule termIndex(#offset(_,_)) => 100050
rule termIndex(#reallocate(_,_)) => 100051
rule termIndex(#AllocateInit(_,_,_)) => 100052
rule termIndex(#Allocate(_,_)) => 100053
rule termIndex(#Index(_,_)) => 100054


endmodule


