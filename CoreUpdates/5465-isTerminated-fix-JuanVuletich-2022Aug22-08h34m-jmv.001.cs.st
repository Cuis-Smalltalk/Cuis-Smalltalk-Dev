'From Cuis 6.0 [latest update: #5464] on 22 August 2022 at 8:44:14 am'!

!InstructionStream methodsFor: 'decoding' stamp: 'jmv 8/22/2022 08:36:54'!
atEnd: aPC

	^ aPC > self method endPC! !


!ContextPart methodsFor: 'query' stamp: 'jmv 8/22/2022 08:40:24'!
isDeadOrAtEnd
	"Doing
	aMethodContext isDead or: [aMethodContext isAtEnd]
	can occasionally break. It is possible for pc to become nil in between the calls."

	^ pc
		ifNil: [ true ]
		ifNotNil: [ :aPC | self atEnd: pc ]! !


!MethodContext methodsFor: 'testing' stamp: 'jmv 8/22/2022 08:37:03'!
atEnd: aPC
	^ self isExecutingBlock
		ifTrue: [ self closure startpc + self closure size - 1 = aPC ]
		ifFalse: [ aPC >= self method endPC ]! !


!InstructionStream methodsFor: 'decoding' stamp: 'jmv 8/22/2022 08:37:22'!
atEnd

	^ self atEnd: pc! !

!InstructionStream methodsFor: 'scanning' stamp: 'jmv 8/22/2022 08:35:06'!
peekInstruction
	"Return the next bytecode instruction as a message that an InstructionClient would understand.  The pc remains unchanged."

	| currentPc instr |
	currentPc _ pc.
	instr _ self nextInstruction.
	pc _ currentPc.
	^ instr! !


!Process methodsFor: 'testing' stamp: 'jmv 8/22/2022 08:40:59'!
isTerminated
	"Answer if the receiver is terminated. A process is considered terminated
	if the suspendedContext is the bottomContext and the pc is at the endPC,
	or if all sender context are at the endPC, up to the bottomContext."

	| ctx |
	self isRunning ifTrue: [^ false].
	suspendedContext isNil ifTrue: [ ^true ].
	ctx _ suspendedContext.
	[
		"Any context that can further run means we're not terminated"
		ctx isDeadOrAtEnd ifFalse: [ ^false ].
		"But if we already checked the whole chain, we're done"
		ctx isBottomContext ifTrue: [ ^true ].
		ctx _ ctx sender ] repeat.! !

!methodRemoval: MethodContext #atEnd stamp: 'jmv 8/22/2022 08:37:27'!
MethodContext removeSelector: #atEnd!
