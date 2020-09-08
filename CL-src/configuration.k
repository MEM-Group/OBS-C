

module CONFIGURATION

import DOMAINS

syntax ExpCL

configuration
<T>
   <threads>
     <Thread multiplicity="*" type="Map">
        <tname> "main" </tname>
	<threadId> 0:Int  </threadId>
        <k> $PGM:ExpCL </k>
        <env> .Map </env>
        <clstack> .List </clstack>
     </Thread>
     <activeThreads> .Set </activeThreads>
   </threads>

   <closures>
        <closureCnt> 0:Int </closureCnt>
        <funclosure> .Map </funclosure>
        <closure multiplicity = "*" type="Map">
             <crId> 0:Int </crId>
             <crContext> .K </crContext>
             <crParams> .K </crParams>
             <crBody> .K </crBody>
        </closure>
    </closures>
   <memory>
     <slocAddr> 0:Int </slocAddr>
     <sloc> .Map  </sloc>
     <blkAddr> 0:Int </blkAddr>
     <blocks>
	.Map
     </blocks>
     <clk> 0:Int </clk>
   </memory>
   <output> .List  </output>
</T>

endmodule


