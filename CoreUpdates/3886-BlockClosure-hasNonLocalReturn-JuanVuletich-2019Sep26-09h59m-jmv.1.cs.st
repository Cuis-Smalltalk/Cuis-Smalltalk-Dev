'From Cuis 5.0 [latest update: #3885] on 26 September 2019 at 10:00:23 am'!

!BlockClosure methodsFor: 'accessing' stamp: 'jmv 9/25/2019 09:16:04'!
capturedValues
	| n copiedValues |
	n _ self numCopiedValues.
	copiedValues _ Array new: n.
	1 to: n do: [ :i |
		copiedValues at: i put: (self copiedValueAt: i) ].
	^copiedValues! !

!BlockClosure methodsFor: 'accessing' stamp: 'jmv 9/26/2019 09:19:46'!
endpc
	"Determine end of block from the instruction preceding it.
	 Find the instruction by using an MNU handler to capture
	 the instruction message sent by the scanner."
	| myMethod scanner preceedingBytecodeMessage end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: myMethod initialPC.
	[scanner pc < startpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| preceedingBytecodeMessage := ex message]].
	end := preceedingBytecodeMessage arguments last + startpc - 1.
	^end! !

!BlockClosure methodsFor: 'testing' stamp: 'jmv 9/26/2019 09:41:53'!
sendsToSuper
	"Answer whether the receiver sends any message to super."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		(byte = 16r85 or: [
			byte = 16r84 and: [scanner followingByte between: 16r20 and: 16r3F]])
				or: [scanner pc > end]].
	^scanner pc <= end! !


!BlockClosure methodsFor: 'testing' stamp: 'jmv 9/26/2019 09:41:42'!
hasNonLocalReturn
	"Answer whether the receiver has a method-return ('^') in its code."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [:byte | (byte between: 120 and: 124) or: [scanner pc > end]].
	^scanner pc <= end! !

