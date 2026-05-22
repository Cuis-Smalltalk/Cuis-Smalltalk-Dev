Class {
	#name : 'NumberTest',
	#superclass : 'AbstractNumberTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'support' }
NumberTest >> assert: expected is: actual [
	^ self
		assert: (expected == actual)
		description: (self comparingStringBetween: expected and: actual)
]

{ #category : 'support' }
NumberTest >> assertThat: aNumber isEqualTo: anExpectedNumber [

	| result |
	result := self subject: aNumber.
	
	self assert: anExpectedNumber equals: result
]

{ #category : 'support' }
NumberTest >> subject: aString [

	| aStream |
	aStream := ReadStream on: aString.

	^ Number readFrom: aStream
]

{ #category : 'test' }
NumberTest >> test01readFromWhenTheInitilValueIsNaNTheResultIsNaN [

	|initialValue nan result |
	
	nan := Float nan.
	initialValue := 'NaN'.
	result := self subject: initialValue.
	
	self assert: result is: nan
]

{ #category : 'test' }
NumberTest >> test02readFromWhenTheInitilValueIsInfinityTheResultIsInfinity [
	
	self assertThat: 'Infinity' isEqualTo: Float infinity
]

{ #category : 'test' }
NumberTest >> test03readFromWhenTheInitilValueIsMinusInfinityTheResultIsNegativeInfinity [

	self assertThat: '-Infinity' isEqualTo: Float negativeInfinity
]

{ #category : 'test' }
NumberTest >> test04readFromWhenTheInitilValueIsANegativeNumberTheResultIsThatNumberNegated [
	
	self assertThat: '-3' isEqualTo: -3
]

{ #category : 'test' }
NumberTest >> test05readFromWhenTheInitilValueIsJustANumberTheResultIsThatNumber [
	
	self assertThat: '3' isEqualTo: 3
]

{ #category : 'test' }
NumberTest >> test06readFromWhenTheInitialValueIncludesRadixItIsConvertedWithTheIndicatedRadixBase [
	|radixBase number initualNumber |
	
	radixBase := '2r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	self assert: initualNumber asNumber equals: 30
]

{ #category : 'test' }
NumberTest >> test07readFromWhenTheInitialValueIncludesAnInvalidRadixItRaisesAnError [
	|radixBase number initualNumber |
	
	radixBase := '0r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	
	self should: [ initualNumber asNumber ] raise: Error description: 'Invalid radix'
]

{ #category : 'test' }
NumberTest >> test08readFromWhenTheInitialValueIncludesANegativeRadixItConvertsTheNumberWithTheIndicatedRadixBase [
	|radixBase number initualNumber |
	
	radixBase := '-2r'.
	number := '11110'.
	
	initualNumber := radixBase, number.
	
	self assert: initualNumber asNumber equals: -30
]

{ #category : 'test' }
NumberTest >> test09readFromWhenTheInitialValueIsEmptyItRaisesAnError [
	
	self should: [ Number readFrom: '' ] raise: Error description: 'At least one digit expected here'
]

{ #category : 'tests' }
NumberTest >> testExactLog2 [
	-10 to: 10 do: [:i | self assert: i equals: (2 raisedToInteger: i) log2].
	Float emin - Float precision + 1 to: Float emax do: [:i | self assert: i equals: (1.0 timesTwoPower: i) log2].
]

{ #category : 'tests' }
NumberTest >> testFractionPart [

	self 
		assert: 2 fractionPart = 0;
		assert: (1/2) fractionPart = (1/2);
		assert: (4/3) fractionPart = (1/3);
		assert: 2.0 fractionPart = 0.0;
		assert: 0.5 fractionPart = 0.5;
		assert: 2.5 fractionPart = 0.5

]

{ #category : 'tests' }
NumberTest >> testHash1 [
	"Hash should be the same for equivalent instances of various Number classes"

	| boxedFloat float hash largeInteger smallInteger |
	largeInteger := (LargePositiveInteger new: 4)
			digitAt: 1 put: 1;
			digitAt: 2 put: 2;
			digitAt: 3 put: 3;
			digitAt: 4 put: 4;
			yourself.
	smallInteger := largeInteger normalize.
	float := smallInteger asFloat.
	boxedFloat := BoxedFloat64 new
		basicAt: 1 put: (float basicAt: 1);
		basicAt: 2 put: (float basicAt: 2); yourself.
	
	hash := largeInteger hash.
	self assert: smallInteger hash = hash.
	self assert: float hash = hash.
	self assert: boxedFloat hash = hash
]

{ #category : 'tests' }
NumberTest >> testHash2 [
	"Hash should be the same for equivalent instances of various Number classes"

	| boxedFloat float hash largeInteger smallIntIn64ButLargeIntIn32Bits |
	largeInteger := (LargePositiveInteger new: 4)
			digitAt: 1 put: 0;
			digitAt: 2 put: 0;
			digitAt: 3 put: 0;
			digitAt: 4 put: 64;
			yourself.
	smallIntIn64ButLargeIntIn32Bits := largeInteger normalize.
	float := smallIntIn64ButLargeIntIn32Bits asFloat.
	boxedFloat := BoxedFloat64 new
		basicAt: 1 put: (float basicAt: 1);
		basicAt: 2 put: (float basicAt: 2); yourself.
	
	hash := largeInteger hash.
	self assert: smallIntIn64ButLargeIntIn32Bits hash = hash.
	self assert: float hash = hash.
	self assert: boxedFloat hash = hash
]

{ #category : 'tests' }
NumberTest >> testHash3 [
	"Hash should be the same for equivalent instances of various Number classes"

	| boxedFloat float hash largeInteger smallIntIn64ButLargeIntIn32Bits |
	largeInteger := (LargePositiveInteger new: 4)
			digitAt: 1 put: 1;
			digitAt: 2 put: 2;
			digitAt: 3 put: 3;
			digitAt: 4 put: 80;
			yourself.
	smallIntIn64ButLargeIntIn32Bits := largeInteger normalize.
	float := smallIntIn64ButLargeIntIn32Bits asFloat.
	boxedFloat := BoxedFloat64 new
		basicAt: 1 put: (float basicAt: 1);
		basicAt: 2 put: (float basicAt: 2); yourself.
	
	hash := largeInteger hash.
	self assert: smallIntIn64ButLargeIntIn32Bits hash = hash.
	self assert: float hash = hash.
	self assert: boxedFloat hash = hash
]

{ #category : 'tests' }
NumberTest >> testIntegerPart [

	self 
		assert: 2 integerPart = 2;
		assert: (1/2) integerPart = 0;
		assert: (4/3) integerPart = 1;
		assert: 2.0 integerPart = 2.0;
		assert: 0.5 integerPart = 0.0;
		assert: 2.5 integerPart = 2.0

]

{ #category : 'tests' }
NumberTest >> testLog2doesNotOverflow [
	"Note: though this is not a strict identity, we can use strict Float equality here"
	self assert: 3000.0 equals: ((1 bitShift: 3000) - 1) log2.
	self assert: 1500.0 equals: (((1 bitShift: 3000) - 1) / (1 bitShift: 1500)) log2.
]

{ #category : 'tests' }
NumberTest >> testLog2doesNotUnderflow [
	"Note: though this is not a strict identity, we can use strict Float equality here"
	self assert: -2000.0 equals: ((1 bitShift: 2000) - 1) reciprocal log2
]

{ #category : 'tests' }
NumberTest >> testOne [

	self 
		assert: Integer one = 1;
		assert: Float one = 1.0;
		assert: Fraction one = 1
]

{ #category : 'tests' }
NumberTest >> testPrintShowingDecimalPlaces [

	self assert: (String streamContents: [ :s | 111.2 printOn: s fractionDigits: 2]) = '111.20'.
	self assert: (String streamContents: [ :s | 111.2 printOn: s fractionDigits: 0]) = '111'.
	self assert: (String streamContents: [ :s | 111 printOn: s fractionDigits: 0]) = '111'.
	self assert: (String streamContents: [ :s | 111111111111111 printOn: s fractionDigits: 2]) = '111111111111111.00'.
	self assert: (String streamContents: [ :s | 10 printOn: s fractionDigits: 20]) ='10.00000000000000000000'.
	self assert: (String streamContents: [ :s | 0.98 printOn: s fractionDigits: 2]) = '0.98'.
	self assert: (String streamContents: [ :s | -0.98 printOn: s fractionDigits: 2]) = '-0.98'.
	self assert: (String streamContents: [ :s | 2.567 printOn: s fractionDigits: 2]) = '2.57'.
	self assert: (String streamContents: [ :s | -2.567 printOn: s fractionDigits: 2]) = '-2.57'
]

{ #category : 'tests' }
NumberTest >> testPrintShowingDecimalPlaces2 [
	"This tests problems related to Float>>rounded and Float>>roundTo::
	- Float>>#rounded is inexact
	- Float>>#roundTo: might overflow"

	"This number is represented exactly asFloat, it should print exactly"
	self assert: 5000000000000001.0 asTrueFraction = 5000000000000001.
	self assert: 5000000000000001 highBit = 53.
	self assert: (5000000000000001.0 printStringFractionDigits: 0) = '5000000000000001'.
	
	"50000000000001.25 asTrueFraction = (200000000000005/4).
	200000000000005 highBit = 48, 4 isPowerOfTwo,
	So this number is also represented exactly as Float, it should print exactly.
	Beware: (50000000000001.25 / 0.01) rounded exhibit the same problem as above."
	self assert: (String streamContents: [ :s | 50000000000001.25 printOn: s fractionDigits: 2]) = '50000000000001.25'.

	"Since 4 isPowerOfTwo, this number is also represented exactly as Float, it should print exactly"
	self assert: 50000000000001.25 asTrueFraction = (200000000000005/4).
	self assert: 200000000000005 highBit = 48.
	self assert: (50000000000001.25 printStringFractionDigits: 2) = '50000000000001.25'.
	
	"This number is close to fmax"
	self shouldnt: [String streamContents: [ :s | 1.0e306 printOn: s fractionDigits: 3]] raise: Error.
]

{ #category : 'tests' }
NumberTest >> testPrintShowingDecimalPlaces3 [
	"This problem were reported at http://bugs.squeak.org/view.php?id=7028
	unfortunate inversion of left / right padding"
	self assert: (String streamContents: [ :s | 1.009 printOn: s fractionDigits: 3]) = '1.009'.
	self assert: (String streamContents: [ :s | 35.900 printOn: s fractionDigits: 3]) = '35.900'.
	self assert: (String streamContents: [ :s | -0.097 printOn: s fractionDigits: 3]) = '-0.097'.
]

{ #category : 'tests' }
NumberTest >> testRaisedTo [
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"
	
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.
]

{ #category : 'tests' }
NumberTest >> testRaisedToInteger [

	self 
		assert: (2 raisedToInteger: 0) = 1;
		assert: (2 raisedToInteger: 1) = 2;
		assert: (2 raisedToInteger: 4) = 16;
		assert: (0 raisedToInteger: 0) = 1;
		assert: (0 raisedToInteger: 2) = 0;
		assert: (2 raisedToInteger: -1) = (1/2);
		assert: (2 raisedToInteger: -4) = (1/16).
	
	self 
		assert: (-3 raisedTo: 0) = 1;
		assert: (-3 raisedTo: 1) = -3;
		assert: (-3 raisedTo: 2) = 9;
		assert: (-3 raisedTo: 3) = -27;
		assert: (-3 raisedTo: -2) = (1/9);
		assert: (-3 raisedTo: -3) = (-1/27).
	
	self should: [ 0 raisedTo: -1 ] raise: ZeroDivide
]

{ #category : 'tests' }
NumberTest >> testRaisedToIntegerWithFloats [

	self 
		assert: (2.0 raisedToInteger: 0) = 1.0;
		assert: (2.0 raisedToInteger: 1) = 2.0;
		assert: (2.0 raisedToInteger: 4) = 16.0;
		assert: (0.0 raisedToInteger: 0) = 1.0;
		assert: (0.0 raisedToInteger: 2) = 0.0;
		assert: (2.0 raisedToInteger: -1) = 0.5;
		assert: (2.0 raisedToInteger: -4) = 0.0625;
		assert: (Float e raisedToInteger: -746) = 0.0;
		assert: (Float e raisedToInteger: -745) = 0.0 successor;
		assert: (Float e raisedToInteger: -744) = 0.0 successor successor.	
	self 
		assert: (-3.0 raisedToInteger: 0) = 1.0;
		assert: (-3.0 raisedToInteger: 1) = -3.0;
		assert: (-3.0 raisedToInteger: 2) = 9.0;
		assert: (-3.0 raisedToInteger: 3) = -27.0;
		assert: (-2.0 raisedToInteger: -2) = 0.25;
		assert: (-2.0 raisedToInteger: -3) = -0.125;
		assert: (Float e negated raisedToInteger: -746) = 0.0;
		assert: (Float e negated raisedToInteger: -745) = -0.0 predecessor;
		assert: (Float e negated raisedToInteger: -744) = 0.0 successor successor.
	
	self should: [ 0.0 raisedToInteger: -1 ] raise: ZeroDivide
]

{ #category : 'tests' }
NumberTest >> testReadFrom [
	
	self assert: 1.0e-14	= (Number readFrom: '1.0e-14').
	self assert: 2r1e26	= (Number readFrom: '2r1e26').
]

{ #category : 'tests' }
NumberTest >> testReciprocal [

	self 
		assert: 1 reciprocal = 1;
		assert: 2 reciprocal = (1/2);
		assert: -1 reciprocal = -1;
		assert: -3 reciprocal = (-1/3).
		
	self should: [ 0 reciprocal ] raise: ZeroDivide
]

{ #category : 'tests' }
NumberTest >> testZeroDivideHandler [
	"Test for user-defined ZeroDivide behavior"

	[
		self assert: 1 / 0 = Float infinity.
		self assert: -1 / 0 = Float negativeInfinity.
		self assert: 1.0 / 0 = Float infinity.
		self assert: -1.0 / 0 = Float negativeInfinity.
		self assert: 1 / 0.0 = Float infinity.
		self assert: -1 / 0.0 = Float negativeInfinity.
		self assert: 1.0 / 0.0 = Float infinity.
		self assert: -1.0 / 0.0 = Float negativeInfinity.
	]	on: ZeroDivide
		do: [:ex | ex resume: ex receiver sign * Float infinity ]

]
