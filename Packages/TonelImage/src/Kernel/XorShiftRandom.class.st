"
I implement XorShift pseudo random number generators invented by George Marsaglia.
See https://en.wikipedia.org/wiki/Xorshift . Paper at https://www.jstatsoft.org/article/view/v008i14/916
Most implementations hardcode the a, b, c parameters, and work either on 32 or 64 bit space.
This one allows chosing them. Still, a and c are right shifts and b is a left shift.
"
Class {
	#name : 'XorShiftRandom',
	#superclass : 'Random',
	#instVars : [
		'a',
		'b',
		'c',
		'mask',
		'last'
	],
	#category : 'Kernel-Numbers',
	#package : 'Kernel',
	#tag : 'Numbers'
}

{ #category : 'instance creation' }
XorShiftRandom class >> a: firstShift b: secondShift c: thirdShift bits: bitsSize [
	^self basicNew a: firstShift b: secondShift c: thirdShift bits: bitsSize
]

{ #category : 'visualizations' }
XorShiftRandom class >> display22BitsBlackWhite [
	"
	self display22BitsBlackWhite
	"
	| bits f generator h w |
	generator := XorShiftRandom a: -2 b: 7 c: -3 bits: 22.
	w := 2048.
	h := 2048.
	bits := ByteArray new: w*h // 8.
	f := Form extent: w@h depth: 1 bits: bits.
	bits atAllPut: 0.
	(2 raisedTo: 22)-1 timesRepeat: [
		bits bitAt: generator nextInteger put: 1.
		Random next > 0.9999 ifTrue: [
			f display. Display forceToScreen ]].
	f display. Display forceToScreen.
]

{ #category : 'visualizations' }
XorShiftRandom class >> display22BitsColor [
	"
	self display22BitsColor
	"
	| bits f generator h w |
	generator := XorShiftRandom a: -2 b: 7 c: -3 bits: 22.
	w := 512.
	h := 512.
	bits := ByteArray new: w*h *16 // 8.
	f := Form extent: w@h depth: 16 bits: bits.
	f fillColor: Color black.
	(2 raisedTo: 22)-1 timesRepeat: [
		bits bitAt: generator nextInteger put: 1.
		Random next > 0.9999 ifTrue: [
			f display. Display forceToScreen ]].
	f display. Display forceToScreen.
]

{ #category : 'parameter search' }
XorShiftRandom class >> lookFor22BitCandidateParameters [
	"XorShift pseudo Random Number Generator. See https://en.wikipedia.org/wiki/Xorshift
	IdentityHash is a 22 bit number. It is easy to find documented parameters for XorShift covering the whole 32 and 64 bit spaces.
	1. But we need to cover the 22 bit space. The following script lists a set of such parameters.
	Explore parameter space to find (a, b, c) parameters that generate all possible hashes (implying a period = hashMask).
	self lookFor22BitCandidateParameters
	"
	| generator value differentValues expectedDifferentValues |
	expectedDifferentValues := (2 raisedTo: 22) - 1.
	-9 to: -1 do: [ :a |
		1 to: 8 do: [ :b |
			-7 to: -1 do: [ :c |
				generator := XorShiftRandom a: a b: b c: c bits: 22.
				differentValues := Set new.
				[
					value := generator nextInteger.
					differentValues includes: value ]
				whileFalse: [
					differentValues add: value ].
				differentValues size = expectedDifferentValues ifTrue: [
					{a. b. c} print ]]]].
]

{ #category : 'parameter search' }
XorShiftRandom class >> pickBest22BitHashParameters [
	"When using this RNG for generating hashes, the next problem we face is that generally we'll not be using the full hash.
	For smaller collections, only the rest of the division by the collection size is actually used.
	For each of the candidates found in #lookFor22BitCandidateParameters, try the rest of the division by 2^k (i.e. take AND with a smaller 'evaluatedMask' and see how many distinct hashes of this size can be generated.
	List those than yield at least half of the full k bits space.
	self pickBest22BitHashParameters
	"
	| candidates |
	candidates := {
		#(-9 5 -7).
		#(-9 5 -3).
		#(-7 7 -3).
		#(-5 3 -2).
		#(-3 7 -7).
		#(-3 7 -2).
		#(-2 3 -5).
		#(-2 7 -3).
	}.
	candidates do: [ :abc |
		| fractions |
		fractions := OrderedCollection new.
		1 to: 21 do: [ :usedMaskSize |
				| evaluatedMask generator hash hashes |
				evaluatedMask := (2 raisedTo: usedMaskSize) - 1.
				generator := XorShiftRandom a: abc first b: abc second c: abc third bits: 22.
				hashes := Set new.
				evaluatedMask timesRepeat: [
					hash := generator nextInteger bitAnd: evaluatedMask.
					hashes add: hash ].
				fractions add: (hashes size / evaluatedMask asFloat) ].
			(fractions min > 0.5 ) ifTrue: [
				{abc. fractions min. fractions average} print ]].
]

{ #category : 'instance creation' }
XorShiftRandom class >> seed: anInteger [
	| answer |
	answer := self a: -2 b: 7 c: -3 bits: 22.
	answer seed: anInteger.
	^answer
]

{ #category : 'initialization' }
XorShiftRandom >> a: firstShift b: secondShift c: thirdShift bits: bitsSize [

	(firstShift < 0 and: [ secondShift > 0 and: [ thirdShift < 0 ]]) ifFalse: [
		self error: 'Invalid XorShift parameters' ].

	a := firstShift.
	b := secondShift.
	c := thirdShift.
	mask := (2 raisedToInteger: bitsSize) - 1.
	last := 1 bitAnd: mask. "If any other value is used, ensure it is not larger than mask"
]

{ #category : 'private' }
XorShiftRandom >> nextChunkBits [
	^self nextInteger
]

{ #category : 'private' }
XorShiftRandom >> nextChunkSize [
	^(mask + 1) log2
]

{ #category : 'accessing' }
XorShiftRandom >> nextInteger [
	last := last bitXor: (last bitShift: a).
	last := last bitXor: ((last bitShift: b) bitAnd: mask).
	last := last bitXor: (last bitShift: c).
	^last
]

{ #category : 'initialization' }
XorShiftRandom >> seed: aNumber [
	last := aNumber.
	mask ifNotNil: [ last := last bitAnd: mask ].
]
