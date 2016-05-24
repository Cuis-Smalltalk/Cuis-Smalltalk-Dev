'From Cuis 4.2 of 25 July 2013 [latest update: #2775] on 24 May 2016 at 10:00:26.388435 am'!

!ParkMiller88Random methodsFor: 'private' stamp: 'len 5/20/2016 02:23'!
initialize
	"Output stabilization is the user's responsibility"

	[
		seed _ (Time localMillisecondClock + self identityHash) hashMultiply \\ self m.
		seed = 0  "zero seeds are unacceptable"
	] whileTrue.
	seed _ seed asFloat! !

