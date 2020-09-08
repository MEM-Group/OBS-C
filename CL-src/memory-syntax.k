
require "configuration.k"

module MEMORY-SYNTAX

import DOMAINS
import CONFIGURATION


syntax KMEM
       ::= #blkInfo(Int)
         | #lo(Int,ValueCL)


syntax KResult
       ::= ValueCL
         | ValueCLs

syntax Uninit
       ::= #uninit(Int)

syntax ValueCL
       ::= Int
         | Bool
         | String
         | Float
         | "#unit"
         | cr(Int)
	 | BlkAddr
	 | #ref(Path)
	 | #tid(Int)
	 | Uninit
	 | #pair(Location,Int)
	 | #slv(StackLoc)

syntax Int ::= ValueIndex(ValueCL) [function]

rule ValueIndex(I:Int) => 0
rule ValueIndex(B:Bool) => 1
rule ValueIndex(S:String) => 2
rule ValueIndex(F:Float) => 3
rule ValueIndex(#unit) => 4
rule ValueIndex(cr(_)) => 5
rule ValueIndex(B:BlkAddr) => 6
rule ValueIndex(#ref(_)) => 7
rule ValueIndex(#tid(_)) => 8
rule ValueIndex(_:Uninit) => 9
rule ValueIndex(#pair(_,_)) => 10
rule ValueIndex(#slv(_)) => 11

syntax Location
       ::= #heap(Int,Int)
         | StackLoc

syntax HeapLoc
       ::= #heap(Int,Int)

syntax StackLoc
       ::= #stack(Int)


syntax Path
       ::= Location
	 | #deref(Path)
	 | #field(Path,Int)



syntax ValueCLs
       ::= List{ValueCL, ","}		[klabel(#ValueCLs)]

syntax BlkAddr
       ::= #blk(Int)

syntax MemOp
       ::= #allocate(Int, ValueCLs)
         | #allocStack(Int)
	 | #lv(Path,Int)
	 | #read(Path)
	 | #write(Path, ValueCL)
         | #free(BlkAddr)
 	 | #move(Path)

syntax MemOps
       ::= List{MemOp, ""}

endmodule






















