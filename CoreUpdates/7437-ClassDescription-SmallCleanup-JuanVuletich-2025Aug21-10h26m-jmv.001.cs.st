'From Cuis7.5 [latest update: #7433] on 21 August 2025 at 10:31:43 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/21/2025 10:28:29'!
reformatMethodAt: selector 
	| newCodeString method | 
	newCodeString := (self compilerClass new)
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStamp
		inFile: 2 priorMethod: method! !

!methodRemoval: CompiledMethod #putSource:fromParseNode:class:category:inFile:priorMethod: stamp: 'jmv 8/21/2025 10:28:46'!
CompiledMethod removeSelector: #putSource:fromParseNode:class:category:inFile:priorMethod:!
!methodRemoval: ClassDescription #printCategoryChunk:on:priorMethod: stamp: 'jmv 8/21/2025 10:28:59'!
ClassDescription removeSelector: #printCategoryChunk:on:priorMethod:!
