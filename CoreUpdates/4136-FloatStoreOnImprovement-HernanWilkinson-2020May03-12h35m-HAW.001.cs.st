'From Cuis 5.0 [latest update: #4132] on 3 May 2020 at 3:16:45 pm'!

!BoxedFloat64 methodsFor: 'printing' stamp: 'HAW 5/3/2020 15:05:51'!
storeOn: aStream base: base 

	self isFinite
		ifTrue: [ super storeOn: aStream base: base ]
		ifFalse: [ self isNaN
			ifTrue: [aStream nextPutAll: 'Float nan']
			ifFalse: [self > 0.0
					ifTrue: [aStream nextPutAll: 'Float infinity']
					ifFalse: [aStream nextPutAll: 'Float infinity negated']]]! !


!Float methodsFor: 'printing' stamp: 'HAW 5/3/2020 15:06:05'!
storeOn: aStream base: base 
	
	"Print the Number exactly so it can be interpreted back unchanged"
	
	self sign = -1 ifTrue: [aStream nextPutAll: '-'].
	base = 10 ifFalse: [aStream print: base; nextPut: $r].
	self isZero
		ifTrue: [aStream nextPutAll: '0.0']
		ifFalse: [self abs absPrintExactlyOn: aStream base: base]! !


!BoxedFloat64 reorganize!
('arithmetic' * + - / arg argument)
('comparing' < <= = > >= ~=)
('mathematical functions' arcTan exp ln primSqrt raisedTo: raisedToInteger: sin timesTwoPower:)
('truncation and round off' exponent fractionPart truncated)
('testing' isDenormalized isFinite isInfinite isNaN sign)
('*BaseImageTests-Kernel-Numbers' primTestAdd: primTestArcTan primTestDivideBy: primTestEqual: primTestExp primTestExponent primTestGreater: primTestGreaterEqual: primTestLess: primTestLessEqual: primTestLn primTestMultiplyBy: primTestNotEqual: primTestSin primTestSqrt primTestSubtract: primTestTruncated)
('printing' storeOn:base:)
!

