'From Cuis 5.0 of 7 November 2016 [latest update: #3805] on 22 June 2019 at 3:27:28 pm'!

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 6/22/2019 14:40:41'!
/= anObject

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide signalWithDividend: self]
		ifBothZero: [ZeroDivide signalWithDividend: self]! !


!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 6/22/2019 14:40:49'!
/= anObject

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide signalWithDividend: self]
		ifBothZero: [ZeroDivide signalWithDividend: self]! !

