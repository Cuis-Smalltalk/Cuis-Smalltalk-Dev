'From Cuis 6.0 [latest update: #5259] on 10 June 2022 at 9:35:30 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/10/2022 21:29:53'!
adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with a number, convert me to a number."

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'converting' stamp: 'Squeak1.0 9/20/1996 10:50:05'!
truncateTo: smallSize
	"return myself or a copy shortened to smallSize.  1/18/96 sw"

	^ self size <= smallSize
		ifTrue:
			[self]
		ifFalse:
			[self copyFrom: 1 to: smallSize]! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/28/2015 10:03'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a number, convert me to a number."

	self shouldNotImplement.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 5/28/2015 10:03'!
adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a point, convert me to a number."

	self shouldNotImplement.! !

!methodRemoval: String #truncateTo: stamp: 'jmv 6/10/2022 21:35:03'!
String removeSelector: #truncateTo:!
!methodRemoval: String #adaptToCollection:andSend: stamp: 'jmv 6/10/2022 21:30:54'!
String removeSelector: #adaptToCollection:andSend:!
