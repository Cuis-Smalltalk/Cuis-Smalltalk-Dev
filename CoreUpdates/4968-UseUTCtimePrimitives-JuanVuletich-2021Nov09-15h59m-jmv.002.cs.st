'From Cuis 5.0 [latest update: #4967] on 9 November 2021 at 4:02:16 pm'!

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 11/9/2021 15:06:18'!
truncateToSeconds
	nanos _ 0! !

!Time class methodsFor: 'primitives' stamp: 'jmv 11/9/2021 14:26:36'!
primUtcWithOffset: arrayOrObjectWithTwoSlots
	"The parameter may be a two element array, or an object whose first two instance
	variables are expected to be UTC microseconds and seconds offset from GMT.

	First element is set to the number of microseconds since the UTC Posix (Unix) epoch, 
	i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	It might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	
	Second element is set to current seconds offset from GMT in the local time zone.

	Recent VMs implement this. Older Interpreters might not.
	Time primUtcWithOffset: {0. 0} 
	"

	<primitive: 'primitiveUtcWithOffset'>
	^nil! !


!DateAndTime class methodsFor: 'ansi protocol' stamp: 'jmv 11/9/2021 15:28:56'!
now
	"Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as TimeStamps
	DateAndTime now
	"

	| days nanoseconds remainingNanoseconds remainingSeconds utcMicroSsecondsAndOffset |
	utcMicroSsecondsAndOffset _ Time primUtcWithOffset: `{ 0. 0}`.
	nanoseconds _ (utcMicroSsecondsAndOffset first + (utcMicroSsecondsAndOffset second * 1000000)) * 1000.
	(LastTick = nanoseconds)
		ifFalse: [
			LastTick _ nanoseconds]
		ifTrue: [
			LastTickSemaphore critical: [
				LastTick _  LastTick + 1.
				nanoseconds _ LastTick ]].

	days _ nanoseconds // Time nanosecondsInDay.
	remainingNanoseconds _ nanoseconds \\ Time nanosecondsInDay.
	remainingSeconds _ remainingNanoseconds // 1000000000.
	remainingNanoseconds _ remainingNanoseconds \\ 1000000000.

	^ self basicNew
		setJdn: `DateAndTime unixEpoch julianDayNumber` + days
		seconds: remainingSeconds 
		nano: remainingNanoseconds 
		offset: (Duration seconds: utcMicroSsecondsAndOffset second)! !

!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 11/9/2021 15:06:44'!
nowUpToSeconds
	"Resolution is up to one second. Don't use as a TimeStamp!!
	DateAndTime nowUpToSeconds
	"

	^self now truncateToSeconds! !


!Time class methodsFor: 'ansi protocol' stamp: 'jmv 11/9/2021 15:28:41'!
now
	"Answer a Time representing the time right now - this is a 24 hour clock.
	Precision is microsecond."

	| microseconds utcMicroSsecondsAndOffset |
	utcMicroSsecondsAndOffset _ Time primUtcWithOffset: `{ 0. 0}`.
	microseconds _ (utcMicroSsecondsAndOffset first + (utcMicroSsecondsAndOffset second * 1000000)).
	^ self seconds:  (microseconds // 1000000) nanoSeconds: (microseconds \\ 1000000) * 1000.! !

!Time class methodsFor: 'private' stamp: 'jmv 11/9/2021 15:32:45'!
currentUtcOffset
	"
	Time currentUtcOffset
	"
	^ DateAndTime now offset! !

