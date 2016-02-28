'From Cuis 4.2 of 25 July 2013 [latest update: #2690] on 24 February 2016 at 6:24:28.599654 pm'!

!LaggedFibonacciRandom methodsFor: 'private' stamp: 'len 2/22/2016 19:50'!
nextChunkBits
	^ self next mantissaPart bitAnd: 16rFFFFFFFFFFFFF! !

