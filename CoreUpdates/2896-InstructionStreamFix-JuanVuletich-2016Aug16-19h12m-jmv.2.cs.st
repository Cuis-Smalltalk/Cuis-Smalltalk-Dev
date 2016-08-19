'From Cuis 4.2 of 25 July 2013 [latest update: #2895] on 16 August 2016 at 7:38:18 pm'!

!InstructionStream methodsFor: 'decoding' stamp: 'jmv 8/16/2016 19:35:29'!
interpretNextInstructionFor: client
	"Send to the argument, client, a message that specifies the type of the next instruction."

	^self method encoderClass interpretNextInstructionFor: client in: self! !


!Array methodsFor: 'as yet unclassified' stamp: 'jmv 8/16/2016 19:12:06'!
createMethod: a class: b header: c
	"To be deleted cleanly"! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/16/2016 19:16:02'!
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) | (aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header >= 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/16/2016 19:16:10'!
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) | (aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	(self allSubInstances
			detect: [:m| m header < 0 and: [m encoderClass ~~ aBytecodeEncoderSubclass]]
			ifNone: []) ifNotNil:
		[Warning signal: 'There are existing CompiledMethods with a different encoderClass.'].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

