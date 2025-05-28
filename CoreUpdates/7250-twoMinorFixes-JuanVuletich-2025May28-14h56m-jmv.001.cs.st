'From Cuis7.3 [latest update: #7249] on 28 May 2025 at 2:56:26 pm'!

!DateAndTime class methodsFor: 'ansi protocol' stamp: 'jmv 5/27/2025 17:08:02'!
now
	"Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as TimeStamps
	DateAndTime now
	"

	| days nanoseconds remainingNanoseconds remainingSeconds utcMicroSsecondsAndOffset |
	utcMicroSsecondsAndOffset := Time primUtcWithOffset: (Array new: 2).
	nanoseconds := (utcMicroSsecondsAndOffset first + (utcMicroSsecondsAndOffset second * 1000000)) * 1000.
	(LastTick = nanoseconds)
		ifFalse: [
			LastTick := nanoseconds]
		ifTrue: [
			LastTickSemaphore critical: [
				LastTick :=  LastTick + 1.
				nanoseconds := LastTick ]].

	days := nanoseconds // Time nanosecondsInDay.
	remainingNanoseconds := nanoseconds \\ Time nanosecondsInDay.
	remainingSeconds := remainingNanoseconds // 1000000000.
	remainingNanoseconds := remainingNanoseconds \\ 1000000000.

	^ self basicNew
		setJdn: `DateAndTime unixEpoch julianDayNumber` + days
		seconds: remainingSeconds 
		nano: remainingNanoseconds 
		offset: (Duration seconds: utcMicroSsecondsAndOffset second)! !


!Time class methodsFor: 'ansi protocol' stamp: 'jmv 5/27/2025 17:07:38'!
now
	"Answer a Time representing the time right now - this is a 24 hour clock.
	Precision is microsecond."

	| microseconds utcMicroSsecondsAndOffset |
	utcMicroSsecondsAndOffset := Time primUtcWithOffset: (Array new: 2).
	microseconds := (utcMicroSsecondsAndOffset first + (utcMicroSsecondsAndOffset second * 1000000)).
	^ self seconds:  (microseconds // 1000000) nanoSeconds: (microseconds \\ 1000000) * 1000.! !

