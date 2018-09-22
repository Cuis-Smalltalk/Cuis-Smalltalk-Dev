'From Cuis 5.0 of 7 November 2016 [latest update: #3450] on 22 September 2018 at 3:33:36 pm'!

!Number class methodsFor: 'instance creation' stamp: 'jmv 9/22/2018 09:01:25'!
readRemainderOf: integerPart from: aStream base: base withSign: sign
	"Read optional fractional part and exponent, and return the final result"
	| value fraction fracpos peekChar |

	value := integerPart.
	(aStream peekFor: $.)
		ifTrue: [		 "<integer>.<fraction>"
			(aStream atEnd not and: [ aStream peek digitValue between: 0 and: base - 1 ])
				ifTrue:  [
					fracpos := aStream position.
					fraction := Integer readFrom: aStream base: base.
					fraction := fraction / (base raisedTo: aStream position - fracpos).
					value := value asFloat + fraction]
				ifFalse: [
					"oops - just <integer>."
					aStream skip: -1.
					"un-gobble the period"
					^ value * sign"Number readFrom: '3r-22.2'"]].
	peekChar := aStream peek.
	peekChar = $e | (peekChar = $d) | (peekChar = $q)
		ifTrue: [			"<number>(e|d|q)<exponent>>"
			aStream next.
			(aStream atEnd not and: [ (aStream peek digitValue between: 0 and: 9) or: [ aStream peek = $- ]])
				ifTrue:  [
					value := value * (base raisedTo: (Integer readFrom: aStream)) ]
				ifFalse: [
					"oops - just <number>."
					aStream skip: -1.]].
	^(value isFloat
		and: [ value = 0.0 and: [ sign = -1 ]])
			ifTrue: [ Float negativeZero ]
			ifFalse: [ value * sign ]! !

