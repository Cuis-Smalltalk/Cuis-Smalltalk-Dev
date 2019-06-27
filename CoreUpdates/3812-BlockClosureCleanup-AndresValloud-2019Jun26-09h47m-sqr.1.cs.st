'From Cuis 5.0 of 7 November 2016 [latest update: #3811] on 26 June 2019 at 10:17:44 am'!

!BlockClosure methodsFor: 'testing' stamp: 'sqr 6/26/2019 09:59:10'!
hasNonLocalReturn
	"Answer whether the receiver has a method-return ('^') in its code."
	| myMethod scanner preceedingBytecodeMessage end |
	"Determine end of block from the instruction preceding it.
	 Find the instruction by using an MNU handler to capture
	 the instruction message sent by the scanner."
	myMethod := outerContext method.
	scanner := InstructionStream new method: myMethod pc: myMethod initialPC.
	[scanner pc < startpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| preceedingBytecodeMessage := ex message]].
	end := preceedingBytecodeMessage arguments last + startpc - 1.
	scanner method: myMethod pc: startpc.
	scanner scanFor: [:byte | (byte between: 120 and: 124) or: [scanner pc > end]].
	^scanner pc <= end! !


!MethodContext methodsFor: 'accessing' stamp: 'sqr 6/26/2019 09:58:58'!
hasNonLocalReturn
	^closureOrNil hasNonLocalReturn! !


!ContextPart methodsFor: 'system simulation' stamp: 'sqr 6/26/2019 09:59:16'!
runSimulated: aBlock contextAtEachStep: block2
	"Simulate the execution of the argument, aBlock, until it ends. aBlock 
	MUST NOT contain an '^'. Evaluate block2 with the current context 
	prior to each instruction executed. Answer the simulated value of aBlock."
	| current |
	aBlock hasNonLocalReturn
		ifTrue: [self error: 'simulation of blocks with ^ can run loose'].
	current := aBlock asContext.
	current pushArgs: Array new from: self.
	[current == self]
		whileFalse:
			[block2 value: current.
			current := current step].
	^self pop! !

!methodRemoval: MethodContext #hasMethodReturn!
MethodContext removeSelector: #hasMethodReturn!
!methodRemoval: BlockClosure #hasMethodReturn!
BlockClosure removeSelector: #hasMethodReturn!
!methodRemoval: BlockClosure #valueUninterruptably!
BlockClosure removeSelector: #valueUninterruptably!
