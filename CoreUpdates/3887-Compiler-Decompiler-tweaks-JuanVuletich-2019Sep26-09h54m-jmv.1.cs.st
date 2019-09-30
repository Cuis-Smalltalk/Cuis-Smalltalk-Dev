'From Cuis 5.0 [latest update: #3885] on 26 September 2019 at 9:56:21 am'!

!Decompiler methodsFor: 'public access' stamp: 'jmv 9/25/2019 17:51:15'!
decompileBlock: aBlock 
	"Decompile aBlock, returning the result as a BlockNode.  
	Show temp names from source if available."
	"Decompiler new decompileBlock: [3 + 4]"
	| methodNode home |
	(home := aBlock home) ifNil: [^ nil].
	method := home method.
	(home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue:
		[(methodNode := method decompile)
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^node]]].
		 ^self error: 'cannot find block node matching aBlock'].
	^self error: 'can only decompile BlockClosures'! !


!Compiler methodsFor: 'public access' stamp: 'jmv 9/26/2019 09:55:33'!
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method |
	
	class _ (aReceiver ifNotNil: [ aReceiver ] ifNil: [ aContext ifNotNil: [ :c | c receiver ]]) class.
	methodNode _ self compileNoPattern: textOrStream in: class context: aContext notifying: aRequestor ifFail: [^failBlock value].
	method _ methodNode generate.
	"I'm not keeping the source nor the methodNode for back compabibility. 
	The SmalltalkEditor sends the message #evaluateMethod:... which already keep the method node
	 for the debugger to show the right source code - Hernan"
	^self evaluateMethod: method to: aReceiver logged: doLog profiled: doProfile! !

