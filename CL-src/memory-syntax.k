
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

syntax MemOps
       ::= List{MemOp, ""}

endmodule






















