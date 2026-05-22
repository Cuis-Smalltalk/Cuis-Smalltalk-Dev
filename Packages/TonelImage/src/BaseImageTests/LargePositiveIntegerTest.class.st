Class {
	#name : 'LargePositiveIntegerTest',
	#superclass : 'AbstractNumberTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'tests' }
LargePositiveIntegerTest >> test_raisedTo_modulo [
	"The bug was caused because of calling unnormalized #\\\.
	The fix is to use #\\ instead."

	self assert: (5445648384 raisedTo: 3 modulo: 1 << 34) = 0.
	self assert: (5445648384 raisedTo: 4 modulo: 1 << 34) = 0.
]

{ #category : 'tests' }
LargePositiveIntegerTest >> testBitShift [

	"Check bitShift from and back to SmallInts"
	
	1 to: 257 do: [:i | self should: [((i bitShift: i) bitShift: 0-i) = i]].
]

{ #category : 'tests' }
LargePositiveIntegerTest >> testDenormalizedPrintString [
	"Check that an un-normalized instance behaves reasonably."

	| i i0 |
	i := LargePositiveInteger new: 4.
	i basicAt: 2 put: 255.
	self assert: i size = 4.
	self assert: i printString = '65280'.	"256*255"
	self assert: i normalize = 65280.
	self assert: (i normalize isMemberOf: SmallInteger).
	
	i0 := LargePositiveInteger new: 0.
	self assert: i0 size = 0.
	self assert: i0 printString = '0'.
	self assert: i0 normalize = 0.
	self assert: (i0 normalize isMemberOf: SmallInteger)
]

{ #category : 'tests' }
LargePositiveIntegerTest >> testDigitAt [

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	1 to: 20 do: [:i | | digit |
		digit := lpi digitAt: i.
		self assert: digit equals: i]

]

{ #category : 'tests' }
LargePositiveIntegerTest >> testDigitAtPut [

	| lpi |
	lpi := LargePositiveInteger new: 20.
	1 to: 20 do: [:i | lpi digitAt: i put: i].
	self assert: lpi equals: 114605103402541699037609980192546360895434064385

]

{ #category : 'tests' }
LargePositiveIntegerTest >> testDigitLength [

	| lpi |
	lpi := 114605103402541699037609980192546360895434064385.
	self assert: lpi digitLength equals: 20

]

{ #category : 'tests' }
LargePositiveIntegerTest >> testEmptyTemplate [

	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargePositiveInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '0'.
	self assert: i normalize = 0.
	self assert: (i normalize isMemberOf: SmallInteger)
]

{ #category : 'tests' }
LargePositiveIntegerTest >> testLargeSqrtFloor [
	"This test fails if a careless implementation naivly factors out the power of two (remove the trailing zeroes up to lowBit).
	This was the case in a previous Squeak 4.x implementation."

	| large root |
	large := (SmallInteger maxVal << 100 + 1) << 100.
	root := large sqrtFloor.
	self assert: root squared <= large.
	self assert: (root+1) squared > large.
]

{ #category : 'tests' }
LargePositiveIntegerTest >> testMultDicAddSub [
	"self run: #testMultDicAddSub"

	| n f f1 |	
	n := 100.
	f := 100 factorial.
	f1 := f*(n+1).
	n timesRepeat: [f1 := f1 - f].
	self assert: (f1 = f). 

	n timesRepeat: [f1 := f1 + f].
	self assert: (f1 // f = (n+1)). 
	self assert: (f1 negated = (Number readFrom: '-' , f1 printString)).
]

{ #category : 'tests' }
LargePositiveIntegerTest >> testNormalize [
	"self run: #testNormalize"
	"Check normalization and conversion to/from SmallInts"

	self assert: ((SmallInteger maxVal + 1 - 1) = SmallInteger maxVal).
	self assert: (SmallInteger maxVal + 3 - 6) = (SmallInteger maxVal-3).
	self should: ((SmallInteger minVal - 1 + 1) = SmallInteger minVal).
	self assert: (SmallInteger minVal - 3 + 6) = (SmallInteger minVal+3).
]

{ #category : 'tests' }
LargePositiveIntegerTest >> testReplaceFromToWithStartingAt [

	| lpi20 lpi7 |
	lpi20 := LargePositiveInteger new: 20.
	1 to: 20 do: [:i | lpi20 digitAt: i put: i].
	lpi7 := LargePositiveInteger new: 7.
	1 to: 7 do: [:i | lpi7 digitAt: i put: 11 - i].
	lpi20 replaceFrom: 6 to: 10 with: lpi7 startingAt: 2.
	"unmodified digits"
	(1 to: 5) , (11 to: 20) do: [:e | | digit |
		digit := lpi20 digitAt: e.
		self assert: digit equals: e].
	"replaced digits"
	6 to: 10 do: [:e | | digit replacementDigit |
		digit := lpi20 digitAt: e.
		replacementDigit := lpi7 digitAt: e - 4.
		self assert: digit equals: replacementDigit]

]

{ #category : 'tests - mathematical functions' }
LargePositiveIntegerTest >> testSqrt [
	self assert: (SmallInteger maxVal + 1) sqrt equals: (SmallInteger maxVal + 1) asFloat sqrt.
]
