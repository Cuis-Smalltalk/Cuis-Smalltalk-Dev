'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 10:56:47.072268 pm'!

!Set methodsFor: 'private' stamp: 'len 2/26/2016 21:29'!
atNewIndex: index put: anObject
	array at: index put: anObject.
	tally _ tally + 1.
	self fullCheck.
	^ anObject! !

