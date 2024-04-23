'From Cuis6.3 [latest update: #6322] on 22 April 2024 at 11:40:19 am'!

!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 4/22/2024 11:34:02'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^UnicodeString streamContents: [ :stream |
		self doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: aSourceCode		]! !


!Compiler methodsFor: 'public access' stamp: 'HAW 4/22/2024 11:39:43'!
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method sourceCodeWithDoIt |
	
	class := (aReceiver ifNotNil: [ aReceiver ] ifNil: [ aContext ifNotNil: [ :c | c receiver ]]) class.
	sourceCodeWithDoIt := Scanner 		doItSourceCodeFor: textOrStream readStream contents in: aContext.
	methodNode := self compileDoIt: sourceCodeWithDoIt in: class context: aContext notifying: aRequestor ifFail: [^failBlock value].
	method := methodNode generate.
	method methodNode: methodNode.
	
	^self evaluateMethod: method to: aReceiver logged: doLog profiled: doProfile! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'HAW 4/22/2024 11:34:56'!
afterCompiling: aSourceCode do: aBlock for: aReceiver in: anEvaluationContext ifFail: aFailBlock
	
	| methodNode method sourceCode compiler |

	sourceCode := Scanner doItSourceCodeFor: aSourceCode in: anEvaluationContext.
	
	methodNode := [
		compiler := Compiler new.
		compiler		
			compileDoIt: sourceCode
			in: aReceiver class
			context: anEvaluationContext
			notifying: self
			ifFail: [ ^ aFailBlock value ]]
		on: OutOfScopeNotification
		do: [ :ex | ex resume: true ].

	methodNode block returnLast.
	method := methodNode generate.
	method methodNode: methodNode.

	^aBlock value: compiler value: method value: aReceiver value: anEvaluationContext 
! !

!methodRemoval: SmalltalkEditor #doItSourceCodeFor:in: stamp: 'HAW 4/22/2024 11:35:06'!
SmalltalkEditor removeSelector: #doItSourceCodeFor:in:!
!methodRemoval: Scanner class #selectionDoItSourceCodeHeaderWithContext:into: stamp: 'HAW 4/22/2024 11:33:45'!
Scanner class removeSelector: #selectionDoItSourceCodeHeaderWithContext:into:!
