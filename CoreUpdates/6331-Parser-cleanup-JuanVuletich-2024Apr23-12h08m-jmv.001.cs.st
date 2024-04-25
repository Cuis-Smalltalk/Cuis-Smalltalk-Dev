'From Cuis6.3 [latest update: #6328] on 23 April 2024 at 12:16:54 pm'!

!Compiler methodsFor: 'public access' stamp: 'jmv 4/23/2024 11:09:05'!
evaluate: textOrStream in: aContext to: aReceiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method |
	
	class := (aReceiver ifNotNil: [ aReceiver ] ifNil: [ aContext ifNotNil: [ :c | c receiver ]]) class.
	methodNode := self compileNoPattern: textOrStream in: class context: aContext notifying: aRequestor ifFail: [^failBlock value].
	method := methodNode generate.
	method methodNode: methodNode.
	
	^self evaluateMethod: method to: aReceiver logged: doLog profiled: doProfile! !

!Compiler methodsFor: 'private' stamp: 'jmv 4/23/2024 12:13:12'!
translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock optimizeSpecialSends: doOptimizeSpecialSends

"si noPattern == doIt, volar o limpiar"
	^self parser
		optimizeSpecialSends: doOptimizeSpecialSends;
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		doIt: doIt
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !


!Parser methodsFor: 'error correction' stamp: 'jmv 4/23/2024 12:11:28'!
sourceDelta

	| userSelectionDelta |
	requestor notNil ifTrue: [
		userSelectionDelta := requestor selectionInterval ifEmpty: [0] ifNotEmpty: [ :userSelection | userSelection first-1 ].
		encoder selector = Scanner doItSelector ifTrue: [ 
			^ 0 - userSelectionDelta ].
		encoder selector = Scanner doItInSelector ifTrue: [ 
			^ 0 - userSelectionDelta ]].
	
	^ 0! !

!Parser methodsFor: 'public access' stamp: 'jmv 4/23/2024 12:13:59'!
parse: sourceStream class: class category: aCategory noPattern: noPattern doIt: doIt context: aContext notifying: aRequestor ifFail: aBlock

	"Answer a MethodNode for the argument, sourceStream, that is the root of
	 a parse tree. Parsing is done with respect to the argument, class, to find
	 instance, class, and pool variables; and with respect to the argument,
	 ctxt, to find temporary variables. Errors in parsing are reported to the
	 argument, req, if not nil; otherwise aBlock is evaluated. The argument
	 noPattern is a Boolean that is true if the the sourceStream does not
	 contain a method header (i.e., for DoIts)."

"si noPattern == doIt, volar o limpiar"
	| methNode repeatNeeded myStream sourceCode startPosition |
	
	category := aCategory.
	myStream := sourceStream.
	[
		repeatNeeded := false.
		startPosition := myStream position.
		sourceCode := myStream upToEnd.
		myStream position: startPosition.
		self encoder init: class context: aContext notifying: self.
		"Protect against possible parsing failure"
		doIt ifTrue: [
			(sourceCode beginsWith: Scanner doItInSelector)
				ifTrue: [encoder selector: Scanner doItInSelector]
				ifFalse: [
					(sourceCode beginsWith: Scanner doItSelector)
						ifTrue: [encoder selector: Scanner doItSelector]]].
		self init: myStream notifying: aRequestor failBlock: [^ aBlock value ].
		doitFlag := noPattern.
		failBlock := aBlock.
		[ methNode := self method: noPattern doIt: doIt context: aContext ]
			on: ReparseAfterSourceEditing
			do: [ :ex |
				repeatNeeded := true.
				myStream := requestor text string readStream ].
		repeatNeeded
	] whileTrue: [ encoder := self encoder class new ].

	methNode sourceText: sourceCode.
	"See #xBacktick"
	sentInLiterals do: [ :sym | encoder noteOptimizedSelector: sym ].

	^ methNode! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 4/23/2024 11:10:38'!
afterCompiling: aSourceCode do: aBlock for: aReceiver in: anEvaluationContext ifFail: aFailBlock
	
	| methodNode method compiler |
	methodNode := [
		compiler := Compiler new.
		compiler		
			compileNoPattern: aSourceCode
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

!methodRemoval: Scanner class #sizeOfDoItSourceCodeHeaderWithContext: stamp: 'jmv 4/23/2024 12:11:36'!
Scanner class removeSelector: #sizeOfDoItSourceCodeHeaderWithContext:!
!methodRemoval: Scanner class #doItSourceCodeFor:in: stamp: 'jmv 4/23/2024 12:10:52'!
Scanner class removeSelector: #doItSourceCodeFor:in:!
!methodRemoval: Scanner class #doItSourceCodeHeaderWithContext:into: stamp: 'jmv 4/23/2024 12:11:41'!
Scanner class removeSelector: #doItSourceCodeHeaderWithContext:into:!
!methodRemoval: Compiler #translate:noPattern:doIt:ifFail: stamp: 'jmv 4/23/2024 12:13:14'!
Compiler removeSelector: #translate:noPattern:doIt:ifFail:!
!methodRemoval: Compiler #compileDoIt:in:context:notifying:ifFail: stamp: 'jmv 4/23/2024 12:12:40'!
Compiler removeSelector: #compileDoIt:in:context:notifying:ifFail:!
