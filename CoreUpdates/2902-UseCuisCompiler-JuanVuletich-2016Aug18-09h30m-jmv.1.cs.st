'From Cuis 4.2 of 25 July 2013 [latest update: #2901] on 18 August 2016 at 9:31:04 am'!

!CompiledMethod class methodsFor: 'class initialization' stamp: 'jmv 8/18/2016 17:08:11'!
initialize    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass ifNil: [
		PrimaryBytecodeSetEncoderClass := UseZCompiler
			ifTrue: [ZEncoderForV3PlusClosures]
			ifFalse: [EncoderForV3PlusClosures]].
	SecondaryBytecodeSetEncoderClass ifNil: [
		SecondaryBytecodeSetEncoderClass := UseZCompiler
			ifTrue: [ZEncoderForV3PlusClosures]
			ifFalse: [EncoderForV3PlusClosures]].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
[
	UseZCompiler _ true.
	CompiledMethod installPrimaryBytecodeSet: ZEncoderForV3PlusClosures.
	CompiledMethod installSecondaryBytecodeSet: ZEncoderForV3PlusClosures.
] on: Warning do: [ :ex | ex resume ].
CompiledMethod initialize
!