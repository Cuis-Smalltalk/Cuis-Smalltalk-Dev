'From Cuis 4.2 of 25 July 2013 [latest update: #2774] on 25 May 2016 at 4:33:04.809215 pm'!
!CompiledMethod methodsFor: 'initialization' stamp: 'jmv 5/24/2016 23:53'!
copyWithTrailerBytes: trailer
"Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
"
^ trailer class == CompiledMethodTrailer  ifTrue: [
	self copyWithTrailerBytesTrailer: trailer
] ifFalse: [
	self copyWithTrailerBytesArray: trailer
]! !


!CompiledMethod class methodsFor: 'class initialization' stamp: 'eem 5/12/2014 17:50'!
initialize    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass ifNil:
		[PrimaryBytecodeSetEncoderClass := EncoderForV3PlusClosures].
	SecondaryBytecodeSetEncoderClass ifNil:
		[SecondaryBytecodeSetEncoderClass := EncoderForV3PlusClosures]! !

!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 5/23/2016 22:10'!
newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex

^ trailer class == CompiledMethodTrailer  ifTrue: [
	self newBytes: numberOfBytes trailerBytesTrailer: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
] ifFalse: [
	self newBytes: numberOfBytes trailerBytesArray: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
]! !

CompiledMethod initialize!
