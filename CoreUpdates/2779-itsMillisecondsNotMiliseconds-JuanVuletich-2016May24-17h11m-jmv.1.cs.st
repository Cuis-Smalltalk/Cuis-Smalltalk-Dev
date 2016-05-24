'From Cuis 4.2 of 25 July 2013 [latest update: #2778] on 24 May 2016 at 5:16:21.604424 pm'!

!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 5/24/2016 17:12'!
unixTimeSeconds: secsSinceUnixEpoch milliseconds: millisecs
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	DateAndTime unixTimeSeconds: DateAndTime now unixTimeSeconds
	"
	^ self unixEpoch + secsSinceUnixEpoch seconds + millisecs milliSeconds! !


!Duration methodsFor: 'public protocol' stamp: 'jmv 5/24/2016 17:11'!
totalMilliseconds

	^ (seconds * 1000) + (nanos // 1e6)! !

!methodRemoval: Duration #totalMiliseconds!
Duration removeSelector: #totalMiliseconds!
!methodRemoval: DateAndTime class #unixTimeSeconds:miliseconds:!
DateAndTime class removeSelector: #unixTimeSeconds:miliseconds:!
