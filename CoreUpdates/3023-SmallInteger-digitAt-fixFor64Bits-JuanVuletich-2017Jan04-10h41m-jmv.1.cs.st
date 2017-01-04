'From Cuis 5.0 of 7 November 2016 [latest update: #3022] on 4 January 2017 at 10:42:00 am'!

!SmallInteger methodsFor: 'system primitives' stamp: 'jmv 1/4/2017 10:35:09'!
digitAt: n 
	"Answer the value of an indexable field in the receiver.  LargePositiveInteger uses bytes of base two number, and each is a 'digit' base 256.  Fail if the argument (the index) is not an Integer or is out of bounds."
	n > Smalltalk wordSize ifTrue: [^ 0].
	self < 0
		ifTrue: 
			[self = SmallInteger minVal ifTrue: [
				"Can't negate minVal -- treat specially"
				^ Smalltalk wordSize = 4
					ifTrue: [ #(0 0 0 64) at: n ]
					ifFalse: [ #(0 0 0 0 0 0 0 16) at: n ]].
			^ ((0-self) bitShift: (1-n)*8) bitAnd: 16rFF]
		ifFalse: [^ (self bitShift: (1-n)*8) bitAnd: 16rFF]! !

