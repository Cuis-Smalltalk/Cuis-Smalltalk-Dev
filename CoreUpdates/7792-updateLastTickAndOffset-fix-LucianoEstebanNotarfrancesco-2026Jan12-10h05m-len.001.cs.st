'From Cuis7.7 [latest update: #7791] on 12 January 2026 at 10:06:12 am'!

!DateAndTime class methodsFor: 'primitive access' stamp: 'len 12/Jan/2026 02:41:44'!
updateLastTickAndOffset
	"
	LastTick:
		number of microseconds since the UTC Posix (Unix) epoch, 
		i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	UtcOffsetInSeconds:
		Local time offset from UTC, in seconds.
	"
	| nanoseconds utcMicroSecondsAndOffset |
	utcMicroSecondsAndOffset := Time primUtcWithOffset: (Array new: 2).
	nanoseconds := utcMicroSecondsAndOffset first * 1000.
	LastTick < nanoseconds
		ifTrue: [ LastTick := nanoseconds]
		ifFalse: [ LastTickSemaphore critical: [ LastTick :=  LastTick + 1 ]].
	UtcOffsetInSeconds := utcMicroSecondsAndOffset second! !

