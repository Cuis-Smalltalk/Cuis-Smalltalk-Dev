'From Cuis 4.2 of 25 July 2013 [latest update: #2193] on 24 February 2015 at 12:06:21.882849 pm'!

!Time class methodsFor: 'general inquiries' stamp: 'SqR 2/22/2015 19:30'!
microsecondsToRun: timedBlock 
	"Answer the number of microseconds timedBlock takes to return its value."

	| initialMicroseconds |
	initialMicroseconds _ self localMicrosecondClock.
	timedBlock value.
	^self localMicrosecondClock - initialMicroseconds! !

