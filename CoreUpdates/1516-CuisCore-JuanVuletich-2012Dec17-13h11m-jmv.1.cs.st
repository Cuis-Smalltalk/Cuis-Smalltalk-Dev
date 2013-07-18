'From Cuis 4.1 of 12 December 2012 [latest update: #1514] on 17 December 2012 at 1:15:32 pm'!

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 12/17/2012 13:15'!
rowAtLocation: aPoint
	"return the number of the row at aPoint"

	listItems isEmpty ifTrue: [ ^0 ].
	^aPoint y // font height + 1 min: listItems size max: 1! !

