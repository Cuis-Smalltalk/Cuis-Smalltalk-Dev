'From Cuis 5.0 [latest update: #4954] on 29 October 2021 at 11:13:41 am'!

!Number methodsFor: 'truncation and round off' stamp: 'jmv 10/29/2021 10:57:03'!
roundedHAFZ
	"Answer the integer nearest the receiver.
	Use the rounding rule commonly taught in school."
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero
	See #rounded"

	^(self + (self sign / 2)) truncated.! !


!Float methodsFor: 'truncation and round off' stamp: 'jmv 10/29/2021 11:03:21'!
roundedHAFZ
	"Answer the integer nearest the receiver."
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero
	See #rounded"

	self fractionPart abs < 0.5
		ifTrue: [^self truncated]
		ifFalse: [^self truncated + self sign].! !


!Integer methodsFor: 'truncation and round off' stamp: 'jmv 10/29/2021 10:55:37'!
roundedHAFZ
	"Refer to the comment in Number >> roundedHAFZ."

	^self! !


!Point methodsFor: 'truncation and round off' stamp: 'jmv 10/29/2021 10:52:04'!
roundedHAFZ
	"Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [y isInteger]) ifTrue: [^ self].
	^ x roundedHAFZ @ y roundedHAFZ! !


!Rectangle methodsFor: 'truncation and round off' stamp: 'jmv 10/29/2021 10:52:11'!
roundedHAFZ
	"Answer a Rectangle whose origin and corner are rounded."

	^Rectangle origin: origin roundedHAFZ corner: self corner roundedHAFZ! !

