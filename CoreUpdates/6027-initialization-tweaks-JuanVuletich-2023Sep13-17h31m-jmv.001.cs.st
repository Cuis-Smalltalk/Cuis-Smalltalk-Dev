'From Cuis 6.0 [latest update: #6026] on 13 September 2023 at 5:38:10 pm'!

!SmallInteger class methodsFor: 'constants' stamp: 'jmv 9/13/2023 17:34:32'!
maxVal
	"Answer the maximum value for a SmallInteger."
	maxVal ifNil: [
		SmallInteger initMinValAndMaxVal ].
	^maxVal.! !

!SmallInteger class methodsFor: 'constants' stamp: 'jmv 9/13/2023 17:31:49'!
minVal
	"Answer the minimum value for a SmallInteger."
	minVal ifNil: [
		SmallInteger initMinValAndMaxVal ].
	^minVal.! !


!SystemDictionary methodsFor: 'image' stamp: 'jmv 9/13/2023 17:36:26'!
wordSize
	"Answer the size in bytes of an object pointer or word in the object memory.
	The value does not change for a given image. The value is cached in WordSize to
	avoid the performance overhead of repeatedly consulting the VM.
	Smalltalk wordSize
	"

	WordSize ifNil: [
		WordSize := self vmParameterAt: 40 ].
	^WordSize.! !

