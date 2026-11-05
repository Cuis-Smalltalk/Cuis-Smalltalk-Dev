'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 5:20:29 pm'!

!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 5/Nov/2026, 8:15:04 pm (UTC)'!
nanosecondsTick
	"Nanoseconds since UTC Posix epoch. In UTC. Independent of local time.
	Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as Numeric Identifiers.
	Answer is a LargePositiveInteger.
	
	DateAndTime nanosecondsTick
	"
	| nanoseconds utcMicroSecondsAndOffset answer |
	utcMicroSecondsAndOffset := Time primUtcWithOffset: (Array new: 2).
	nanoseconds := utcMicroSecondsAndOffset first * 1000.
	LastTickSemaphore critical: [
		LastTick < nanoseconds
			ifTrue: [ LastTick := nanoseconds]
			ifFalse: [ LastTick :=  LastTick + 1 ].
		answer := LastTick ].
	^answer! !

!DateAndTime class methodsFor: 'primitive access' stamp: 'jmv 5/Nov/2026, 8:17:26 pm (UTC)'!
updateLastTickAndOffset
	"
	LastTick:
		number of microseconds since the UTC Posix (Unix) epoch, 
		i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	UtcOffsetInSeconds:
		Local time offset from UTC, in seconds.
	"
	| utcMicroSecondsAndOffset |
	utcMicroSecondsAndOffset := Time primUtcWithOffset: (Array new: 2).
	LastTick := utcMicroSecondsAndOffset first * 1000.
	UtcOffsetInSeconds := utcMicroSecondsAndOffset second.! !

