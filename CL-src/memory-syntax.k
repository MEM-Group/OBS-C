
require "importcl.k"

module MEMORY-SYNTAX

import DOMAINS
import TYDEF
import IMPORTCL


syntax KMEM
       ::= #blkInfo(Int,K)
         | #ms(Int,Int)
         | #lockS(K)


syntax KResult
       ::= ValueCL
         | ValueCLs
	 | ErrorState

syntax StackLoc
       ::= #sl(Int)

syntax ValueCL
       ::= Int
         | Bool
         | String
         | Float
         | #point(Path)
         | "#unit"
         | cr(Int)
	 | #own(BlkAddr)
	 | #shr(Int,Int,Path)
	 | #Shr(Path)
	 | #mut(Int,Int,Path)
	 | #Mut(Path)
	 | BlkAddr
	 | #pair(K,K)
	 | #val(K)
	 | RawPoint

syntax ValueCL
       ::= #threadId(Int)
         | "#breakVal"

syntax KCL
       ::= #writecl(Path,KItem)			[strict(2)]

syntax K
       ::= #selectE(Bool,K,K)


syntax ErrorState
       ::= #uninit(Int)


syntax RawPoint
       ::= #Raw(Location)

syntax KItem
       ::= #lwrite(Path,Int)
         | #lread(Path,Int)


syntax Location
       ::= #heap(Int,Int)
         | StackLoc

syntax ExpCL

syntax Path
       ::= Location
	 | Path "." ExpCL	[klabel(#PFieldExpCL)]
	 | #PArray(Path,ExpCL)	[klabel(#PArray)]
	 | #dRef(Path)
	 | #Brw(Int,Path)


syntax ValueCLs
       ::= List{ValueCL, ","}		[klabel(#ValCLs)]

syntax BlkAddr
       ::= #blk(Int)

syntax MemOp
       ::= #allocateInit(ValueCL, CoreTy, ValueCLs)
         | #allocate(ValueCL,CoreTy)
	 | #allocateInitRaw(ValueCL, CoreTy,ValueCLs)
         | "#allocStack"
         | #free(BlkAddr)
	 | #read(Path)
	 | #write(Path,WriteTerm)
	 | #lv(Path)
	 | #readna(Location)
	 | #readnap(Location)
	 | #writena(Location,WriteTerm)
	 | #writenap(Location,WriteTerm)
//For testing
	 | #Tmatch(Path,Path)
	 | #Assign(Path,Path)
	

syntax WriteTerm
       ::= ValueCL
         | ErrorState

syntax MemOps
       ::= List{MemOp, ""}

syntax WriteTerm
       ::= #clkVal(WriteTerm,Int)			[function]

syntax MemOp
       ::= #freeBlk(Int,Int)

//For verification

syntax Map
       ::= #vecDequeMap(Int,Int)		[function]



endmodule






















