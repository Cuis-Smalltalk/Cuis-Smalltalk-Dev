'From Cuis 4.2 of 25 July 2013 [latest update: #2356] on 9 June 2015 at 3:54:29.630699 pm'!

!Collection methodsFor: 'math functions' stamp: 'jmv 6/9/2015 14:57'!
truncateTo: quantum
	^self collect: [ :each | each truncateTo: quantum ]! !


!Interval methodsFor: 'accessing' stamp: 'jmv 6/9/2015 15:47'!
last 
	"Refer to the comment in SequenceableCollection|last."

"	^stop - (stop - start \\ step)"
	| s |
	s _ self size.
	s = 0 ifTrue: [ ^stop ].
	^self at: s! !

!Interval methodsFor: 'accessing' stamp: 'jmv 6/9/2015 15:52'!
size

	| s |
	s _ step sign.
	stop * s < (start * s )
		ifTrue: [^0].
	^(stop - start / step) rounded + 1! !

!Interval methodsFor: 'enumerating' stamp: 'jmv 6/9/2015 15:49'!
do: aBlock

	1 to: self size do: [ :i |
		aBlock value: (self at: i) ]		! !

!Interval methodsFor: 'enumerating' stamp: 'jmv 6/9/2015 15:49'!
reverseDo: aBlock 
	"Evaluate aBlock for each element of my interval, in reverse order."
	
	self size to: 1 by: -1 do: [ :i |
		aBlock value: (self at: i) ]	! !

!methodRemoval: Interval #valuesInclude:!
Interval removeSelector: #valuesInclude:!
