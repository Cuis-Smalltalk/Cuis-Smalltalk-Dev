'From Cuis 4.2 of 25 July 2013 [latest update: #2777] on 26 May 2016 at 9:21:50.571337 am'!

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 8/11/2016 10:59:24'!
isSpur
	"Answer true if we are a Spur ObjectMemory.
	Spur introduces a new format of header for objects, new format for classes, etc."
	^ self compactClassesArray isNil! !

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 12:28:32'!
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) | (aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/8/2016 12:28:45'!
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[^self].
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) | (aBytecodeEncoderSubclass inheritsFrom: ZBytecodeEncoder)
		ifFalse: [self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder'].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SystemChangeNotifier uniqueInstance initialize.
StrikeFont makeCrVisible.
Smalltalk isSpur ifTrue: [ 		"On Spur, leave Squeak's Compiler active until we adapt Cuis'"
	UseZCompiler _ false.
	CompiledMethod installPrimaryBytecodeSet: EncoderForV3PlusClosures.
	CompiledMethod installSecondaryBytecodeSet: EncoderForV3PlusClosures ]
ifFalse: [ 							"On Non-Spur, leave Cuis' Compiler active."
	UseZCompiler _ true.
	CompiledMethod installPrimaryBytecodeSet: ZEncoderForV3PlusClosures.
	CompiledMethod installSecondaryBytecodeSet: ZEncoderForV3PlusClosures ]!

