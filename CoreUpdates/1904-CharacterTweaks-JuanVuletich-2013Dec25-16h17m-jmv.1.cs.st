'From Cuis 4.2 of 25 July 2013 [latest update: #1898] on 25 December 2013 at 4:24:11.914024 pm'!

!Character methodsFor: 'accessing' stamp: 'jmv 12/25/2013 16:17'!
value
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding."

	^value! !


!SmallInteger methodsFor: 'converting' stamp: 'jmv 12/25/2013 16:23'!
asCharacter
	"Answer the Character whose value is the receiver.
	I believe Spur has a specific primitive for this (jmv)"

	^Character value: self! !


!Character methodsFor: 'accessing' stamp: 'jmv 12/25/2013 16:17'!
asciiValue
	"Answer the value of the receiver that represents its ISO 8859-15 encoding.
	This is the same as ASCII for the first 128 characters."

	^self value! !

!Character methodsFor: 'accessing' stamp: 'jmv 12/25/2013 16:17'!
iso8859s15Code
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding."

	^ self value! !

!Character methodsFor: 'converting' stamp: 'jmv 12/25/2013 16:18'!
asInteger
	"Answer the value of the receiver."

	^self value! !

