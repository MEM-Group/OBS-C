

module CONFIGURATION

import DOMAINS

syntax ExpCL




configuration
<T>
   <threads>
     <Thread multiplicity="*" type="Map">
        <tname> "main" </tname>
        <k> $PGM:ExpCL </k>
        <env> .Map </env>
        <clstack> .List </clstack>
        <threadId> 0:Int  </threadId>
     </Thread>
     <activeThreads> SetItem(0) </activeThreads>
     <cntThreads> 1:Int </cntThreads>
     <cthread> 0:Int </cthread>
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
     <locRW> .Set </locRW>
     <memStatus> .Map </memStatus>
     <clk> 0:Int </clk>
   </memory>

   <output> .List  </output>

</T>

endmodule


