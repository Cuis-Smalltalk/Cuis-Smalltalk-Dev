'From Cuis 5.0 [latest update: #4035] on 29 January 2020 at 9:52:36 am'!

!ClassDescription methodsFor: 'compiling' stamp: 'jmv 1/29/2020 09:51:41'!
compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	| methodAndNode |
	methodAndNode _ self basicCompile: text asString notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	logSource ifTrue: [
		self logMethodSource: text forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: methodAndNode selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector! !

!methodRemoval: ClassDescription #noteCompilationOf:meta: stamp: 'jmv 1/29/2020 09:51:47'!
ClassDescription removeSelector: #noteCompilationOf:meta:!
!methodRemoval: Object class #noteCompilationOf:meta: stamp: 'jmv 1/29/2020 09:51:52'!
Object class removeSelector: #noteCompilationOf:meta:!
