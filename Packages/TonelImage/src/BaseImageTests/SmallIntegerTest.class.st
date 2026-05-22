"
I provide a test suite for SmallInteger values. Examine my tests to see how SmallIntegers should behave, and see how to use them.
"
Class {
	#name : 'SmallIntegerTest',
	#superclass : 'AbstractNumberTest',
	#category : 'BaseImageTests-Kernel-Numbers',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Numbers'
}

{ #category : 'testing - Class Methods' }
SmallIntegerTest >> testBasicNew [

	self should: [SmallInteger basicNew] raise: TestResult exError. 
]

{ #category : 'tests - printing' }
SmallIntegerTest >> testDecimalDigitLength [

	| x length random |
	"Test edge cases"
	x := 1.
	length := 1.
	[ x <= SmallInteger maxVal ] whileTrue: [
		self 
			assert: length equals: x decimalDigitLength;
			assert: (length - 1 max: 1) equals: (x - 1) decimalDigitLength.
		x := x * 10.
		length := length + 1 ].
	"A few values by hand"
	#(
		0 1
		4 1
		12 2
		123 3
		1234 4
		56789 5
		657483 6
		6571483 7
		65174383 8
		625744831 9
		1000001111 10
	), {
		SmallInteger maxVal. Smalltalk wordSize = 8 ifTrue: [ 19 ] ifFalse: [ 10 ]
	}pairsDo: [ :input :expectedOutput |
		self assert: expectedOutput equals: input decimalDigitLength ].
	"Pseudorandom tests."
	random := Random seed: 36rSQUEAK.
	10000 timesRepeat: [
		x := SmallInteger maxVal atRandom: random.
		self assert: x asString size equals: x decimalDigitLength ]
]

{ #category : 'testing - arithmetic' }
SmallIntegerTest >> testDivide [

	self assert: 2 / 1 = 2.
	self assert: (3 / 2) isFraction.
	self assert: 4 / 2 = 2.
	self should: [ 1 / 0 ] raise: ZeroDivide.
]

{ #category : 'tests - arithmetic' }
SmallIntegerTest >> testDivideMayOverflow [
	"Dividing a SmallInteger by another Integer may answer a Large Integer.
	These cases have caused several VM bugs in the past, it's better to keep some assrtion around."
	
	self assert: (SmallInteger minVal / -1) isLarge.
	self assert: (SmallInteger minVal / -1) = (SmallInteger maxVal + 1).
	
	self assert: (SmallInteger minVal quo: -1) isLarge.
	self assert: (SmallInteger minVal quo: -1) = (SmallInteger maxVal + 1).
	
	self assert: (SmallInteger minVal // -1) isLarge.
	self assert: (SmallInteger minVal // -1) = (SmallInteger maxVal + 1).
]

{ #category : 'testing - basic' }
SmallIntegerTest >> testEven [
	
	self assert: (SmallInteger minVal even).
	self deny: (SmallInteger maxVal even).
	
	self deny: ((SmallInteger minVal + 1) even).
	self assert: ((SmallInteger maxVal - 1) even).
	
	self deny: (1 even).
	self deny: (-1 even).
	
	self assert: (2 even).
	self assert: (-2 even).
	
	self assert: (0 even)
]

{ #category : 'testing - Class Methods' }
SmallIntegerTest >> testMaxVal [

	Smalltalk wordSize = 4
		ifTrue: [
			self assert: (SmallInteger maxVal = 16r3FFFFFFF) ]
		ifFalse: [
			self assert: (SmallInteger maxVal = 16rFFFFFFFFFFFFFFF) ]
]

{ #category : 'testing - Class Methods' }
SmallIntegerTest >> testMinVal [

	Smalltalk wordSize = 4
		ifTrue: [
			self assert: (SmallInteger minVal = -16r40000000) ]
		ifFalse: [
			self assert: (SmallInteger minVal = -16r1000000000000000) ]
]

{ #category : 'testing - Class Methods' }
SmallIntegerTest >> testNew [

	self should: [SmallInteger new] raise: TestResult exError. 
]

{ #category : 'testing - basic' }
SmallIntegerTest >> testOdd [
	
	self deny: (SmallInteger minVal odd).
	self assert: (SmallInteger maxVal odd).
	
	self assert: ((SmallInteger minVal + 1) odd).
	self deny: ((SmallInteger maxVal - 1) odd).
	
	self assert: (1 odd).
	self assert: (-1 odd).
	
	self deny: (2 odd).
	self deny: (-2 odd).
	
	self deny: (0 odd)
]

{ #category : 'testing - printing' }
SmallIntegerTest >> testPrintPaddedWith [

	self assert: (123 printStringBase: 2 length: 10 padded: true) = '0001111011'.
	self assert: (123 printStringBase: 8 length: 10 padded: true) = '0000000173'.
	self assert: (123 printStringBase: 10 length: 10 padded: true) = '0000000123'.
	self assert: (123 printStringBase: 16 length: 10 padded: true) = '000000007B'
]

{ #category : 'testing - printing' }
SmallIntegerTest >> testPrintString [
	self assert: 1 printString  = '1'.
	self assert: -1 printString  = '-1'.
	Smalltalk wordSize = 4
		ifTrue: [
			self assert: SmallInteger minVal printString  = '-1073741824'.
			self assert: SmallInteger maxVal printString  = '1073741823' ]
		ifFalse: [
			self assert: SmallInteger minVal printString  =  '-1152921504606846976' .
			self assert: SmallInteger maxVal printString  =  '1152921504606846975'  ].
	self assert: 12345 printString  = '12345'.
	self assert: -54321 printString  = '-54321'.

	self assert: 0 decimalDigitLength = 1.
	self assert: 4 decimalDigitLength = 1.
	self assert: 12 decimalDigitLength = 2.
	self assert: 123 decimalDigitLength = 3.
	self assert: 1234 decimalDigitLength = 4.
	self assert: 56789 decimalDigitLength = 5.
	self assert: 657483 decimalDigitLength = 6.
	self assert: 6571483 decimalDigitLength = 7.
	self assert: 65174383 decimalDigitLength = 8.
	self assert: 625744831 decimalDigitLength = 9.
	self assert: 1000001111 decimalDigitLength = 10.
	Smalltalk wordSize = 4
		ifTrue: [
			self assert: SmallInteger maxVal decimalDigitLength = 10 ]
		ifFalse: [
			self assert: SmallInteger maxVal decimalDigitLength = 19 ]
]

{ #category : 'tests - zero behavior' }
SmallIntegerTest >> testZeroRaisedToNegativePower [
	"this is a test related to http://bugs.squeak.org/view.php?id=6781"

	self should: [0 raisedToInteger: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1] raise: ZeroDivide.
	self should: [0 raisedTo: -1.0] raise: ZeroDivide.

	self should: [-0 raisedToInteger: -1] raise: ZeroDivide.
	self should: [-0 raisedTo: -1] raise: ZeroDivide.
	self should: [-0 raisedTo: -1.0] raise: ZeroDivide.
]
