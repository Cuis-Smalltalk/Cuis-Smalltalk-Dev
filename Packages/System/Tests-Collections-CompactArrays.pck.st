'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 5:18:35 pm'!
'Description '!
!provides: 'Tests-Collections-CompactArrays' 1 1!
!requires: 'Collections-CompactArrays' 1 6 nil!
SystemOrganization addCategory: #'Tests-Collections-CompactArrays'!


!classDefinition: #ShortIntegerArrayTest category: #'Tests-Collections-CompactArrays'!
TestCase subclass: #ShortIntegerArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Collections-CompactArrays'!
!classDefinition: 'ShortIntegerArrayTest class' category: #'Tests-Collections-CompactArrays'!
ShortIntegerArrayTest class
	instanceVariableNames: ''!

!classDefinition: #ShortWordArrayTest category: #'Tests-Collections-CompactArrays'!
TestCase subclass: #ShortWordArrayTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Collections-CompactArrays'!
!classDefinition: 'ShortWordArrayTest class' category: #'Tests-Collections-CompactArrays'!
ShortWordArrayTest class
	instanceVariableNames: ''!


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
