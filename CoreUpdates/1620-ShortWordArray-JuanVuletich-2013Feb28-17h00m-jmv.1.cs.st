'From Cuis 4.1 of 12 December 2012 [latest update: #1619] on 28 February 2013 at 5:01:38 pm'!

!ShortIntegerArray commentStamp: '<historical>' prior: 0!
ShortIntegerArray is an array for efficiently representing signed integers in the 16bit range, i.e. between -16r8000 and 16r7FFF.
Additionaly, using #at:putUnsigned: and #at:putSigned:, it supports unsigned 16 bits integers, between 0 and 16rFFFF. You can also use ShortWordArray to store unsigned 16 bit integers with the usual #at: and #at:put: protocol.!

!classDefinition: #ShortIntegerArrayTest category: #'KernelTests-Collections'!
TestCase subclass: #ShortIntegerArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Collections'!
!classDefinition: #ShortWordArray category: #'Balloon-Collections'!
ShortIntegerArray variableWordSubclass: #ShortWordArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Balloon-Collections'!

!ShortWordArray commentStamp: '<historical>' prior: 0!
WordArrays store 16-bit unsigned Integer values, between 0 and 16rFFFF.!

!classDefinition: #ShortWordArrayTest category: #'KernelTests-Collections'!
TestCase subclass: #ShortWordArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'KernelTests-Collections'!

!ShortIntegerArrayTest methodsFor: 'testing' stamp: 'jmv 2/28/2013 16:58'!
testShortIntegerArray
	"
	Short IntegerArray holds 16 bit signed numbers.
	ShortIntegerArrayTest new testShortIntegerArray
	"
        | u16 |
        u16 := ShortIntegerArray new: 8.

        u16 at: 1 put:  16r7123.
        self assert: ((u16 at: 1) = 16r7123).
        u16 at: 1 put: 0.
        self assert: ((u16 at: 1) = 0).
        u16 at: 1 put:  -1.
        self assert: ((u16 at: 1) = -1).
        u16 at: 1 put:  -32768.
        self assert: ((u16 at: 1) = -32768).
        self should: [u16 at: 1 put:  -32769] raise: Error. "to big"
        self should: [u16 at: 1 put:  (16r7FFF + 1)] raise: Error. "too big"! !


!ShortWordArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:23'!
at: index
	"Return the 16-bit unsigned integer value at the given index of the receiver."

	^self unsignedAt: index! !

!ShortWordArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:23'!
at: index put: value
	"Store the given 16-bit unsigned integer at the given index in the receiver."

	^self at: index putUnsigned: value! !


!ShortWordArrayTest methodsFor: 'testing' stamp: 'jmv 2/28/2013 16:58'!
testShortWordArray
	"
	Short IntegerArray holds 16 bit unsigned numbers.
	ShortWordArrayTest new testShortWordArray
	"
        | u16 |
        u16 := ShortWordArray new: 8.

        u16 at: 1 put:  16r8123.
        self assert: ((u16 at: 1) = 16r8123).
        u16 at: 1 put: 0.
        self assert: ((u16 at: 1) = 0).
        u16 at: 1 put:  32768.
        self assert: ((u16 at: 1) = 32768).
        u16 at: 1 put:  16rFFFF.
        self assert: ((u16 at: 1) = 16rFFFF).
        self should: [u16 at: 1 put:  -1] raise: Error. "wrong sign"
        self should: [u16 at: 1 put:  (16rFFFF + 1)] raise: Error. "too big"! !


!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:21'!
at: index
	"Return the 16-bit signed integer value at the given index of the receiver."

	^self signedAt: index! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:23'!
at: index put: value
	"Store the given 16-bit signed integer at the given index in the receiver."

	^self at: index putSigned: value! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:21'!
at: index putSigned: value
	"Store the given 16-bit signed integer at the given index in the receiver."

	<primitive: 144>
	index isInteger
		ifTrue: [
			(index >= 1 and: [index <= self size])
				ifTrue: [self errorImproperStore]
				ifFalse: [self errorSubscriptBounds: index]].
	index isNumber ifTrue: [^ self at: index truncated put: value].
	self errorNonIntegerIndex.
! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2013 16:54'!
at: index putUnsigned: value
	"Store the given 16-bit unsigned integer at the given index in the receiver."

	| primValue |
	value < 0 ifTrue: [ self errorImproperStore ].
	value > 16rFFFF ifTrue: [ self errorImproperStore ].
	primValue _ value > 16r7FFF
			ifTrue: [
				value - 16r10000.		"Faster. No need for LargeInteger arithmetic"
				"(value bitInvert16 + 1) negated" ]
			ifFalse: [ value ].
	self at: index putSigned: primValue.
	^value! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/27/2013 07:21'!
signedAt: index
	"Return the 16-bit signed integer value at the given index of the receiver."

	<primitive: 143>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber ifTrue: [^ self at: index truncated].
	self errorNonIntegerIndex.
! !

!ShortIntegerArray methodsFor: 'accessing' stamp: 'jmv 2/28/2013 16:52'!
unsignedAt: index
	"Return the 16-bit unsigned integer value at the given index of the receiver."

	| primValue |
	primValue _ self signedAt: index.
	^primValue < 0
		ifTrue: [
			primValue + 16r10000.		"Faster. No need for LargeInteger arithmetic"
			"(primValue + 1) negated bitInvert16" ]
		ifFalse: [ primValue ]! !

