'From Cuis7.3 [latest update: #7154] on 13 May 2025 at 9:27:36 am'!

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 5/13/2025 09:26:11'!
maxAccessibleInstSize
	"Answer the maximum number of accessible named instance variables.
	 This depends on the bytecode set.
	Note: On images without Compiler / Parser / Encoder we'd need a class variable somewhere.
		Assume EncoderForV3PlusClosures if unknown"

	CompiledMethod preferredBytecodeSetEncoderClass isNil
		ifTrue: [ ^256 ].
	^CompiledMethod preferredBytecodeSetEncoderClass maxAccessibleInstSize! !

