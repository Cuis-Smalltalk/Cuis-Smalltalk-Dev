'From Cuis 4.2 of 25 July 2013 [latest update: #2676] on 26 January 2016 at 11:13:16.494327 am'!

!Array2D methodsFor: 'private' stamp: 'jmv 1/26/2016 11:12'!
elementsIndexForI: i j: j
	"Indexes goes from 1 to size (width or height)"

	(j between: 1 and: width) ifFalse: [ self errorSubscriptBounds: j@i ].
	^ i-1*width+j! !

