'From Cuis 4.2 of 25 July 2013 [latest update: #2771] on 20 May 2016 at 3:30:58.248188 pm'!

!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 5/20/2016 14:59'!
unixTimeSeconds: secsSinceUnixEpoch
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	DateAndTime unixTimeSeconds: DateAndTime now unixTimeSeconds
	"
	^ self unixEpoch + secsSinceUnixEpoch seconds! !

!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 5/20/2016 15:26'!
unixTimeSeconds: secsSinceUnixEpoch miliseconds: milisecs
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	DateAndTime unixTimeSeconds: DateAndTime now unixTimeSeconds
	"
	^ self unixEpoch + secsSinceUnixEpoch seconds + milisecs milliSeconds! !

