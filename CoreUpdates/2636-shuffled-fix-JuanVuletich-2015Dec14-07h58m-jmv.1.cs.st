'From Cuis 4.2 of 25 July 2013 [latest update: #2632] on 14 December 2015 at 8:02:28.887471 am'!

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 12/14/2015 07:58'!
shuffleBy: aRandom

	self size to: 1 by: -1 do: [ :i |
		self swap: i with: ((1 to: i) atRandom: aRandom) ]! !


!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 12/14/2015 08:02'!
shuffled
	^ Random withDefaultDo: [ :random |
		"To answer a mutable collection when receiver is, for example, an Interval."
		(self collect: [ :each | each ]) shuffleBy: random ]

"Examples:
	($A to: $Z) shuffled
	(1 to: 20) shuffled
	'This is a String' shuffled
"! !

!methodRemoval: SequenceableCollection #shuffledBy:!
SequenceableCollection removeSelector: #shuffledBy:!
