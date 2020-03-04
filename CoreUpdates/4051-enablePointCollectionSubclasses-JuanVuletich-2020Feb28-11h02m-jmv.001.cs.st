'From Cuis 5.0 [latest update: #4047] on 28 February 2020 at 12:34:49 pm'!

!FloatArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:09:11'!
floatAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!FloatArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:09:39'!
floatAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [self basicAt: index put: value asIEEE32BitWord]
		ifFalse: [self floatAt: index put: value asFloat].
	^value! !


!IntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:22:43'!
integerAt: index
	"Return the integer at the given index"
	| word |
	<primitive: 165>
	word _ self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]! !

!IntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:24:33'!
integerAt: index put: anInteger
	"Store the integer at the given index"
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word _ 16r100000000 + anInteger"
				word _ (anInteger + 1) negated bitInvert32]
		ifFalse:[word _ anInteger].
	self  basicAt: index put: word.
	^anInteger! !


!FloatArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:11:02'!
at: index
	^self floatAt: index! !

!FloatArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:10:34'!
at: index put: value
	^self floatAt: index put: value! !


!IntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:25:30'!
at: index
	^self integerAt: index! !

!IntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:24:58'!
at: index put: anInteger
	^self integerAt: index put: anInteger! !

