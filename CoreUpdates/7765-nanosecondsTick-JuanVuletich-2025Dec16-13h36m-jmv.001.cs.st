'From Cuis7.5 [latest update: #7764] on 16 December 2025 at 2:41:30 pm'!
!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos'
	classVariableNames: 'LastTick LastTickSemaphore UtcOffsetInSeconds'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!

!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 16/Dec/2025 14:41:24'!
nanosecondsTick
	"Nanoseconds since UTC Posix epoch. In UTC. Independent of local time.
	Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as Numeric Identifiers.
	Answer is a LargePositiveInteger.
	
	DateAndTime nanosecondsTick
	"
	self updateLastTickAndOffset.
	^LastTick! !

!DateAndTime class methodsFor: 'primitive access' stamp: 'jmv 16/Dec/2025 14:27:43'!
updateLastTickAndOffset
	"
	LastTick:
		number of microseconds since the UTC Posix (Unix) epoch, 
		i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	UtcOffsetInSeconds:
		Local time offset from UTC, in seconds.
	"
	| nanoseconds utcMicroSsecondsAndOffset |
	utcMicroSsecondsAndOffset := Time primUtcWithOffset: (Array new: 2).
	nanoseconds := utcMicroSsecondsAndOffset first * 1000.
	LastTick = nanoseconds
		ifFalse: [
			LastTick := nanoseconds]
		ifTrue: [
			LastTickSemaphore critical: [
				LastTick :=  LastTick + 1.
				nanoseconds := LastTick ]].
	UtcOffsetInSeconds := utcMicroSsecondsAndOffset second.! !


!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 16/Dec/2025 14:23:07'!
now
	"Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as TimeStamps.
	If, instead, a numeric unique identifier is desired, call #nanosecondsTick
	DateAndTime now
	"

	| nanoseconds utcOffset days remainingNanoseconds remainingSeconds |
	self updateLastTickAndOffset.

	utcOffset := Duration seconds: UtcOffsetInSeconds.
	nanoseconds := UtcOffsetInSeconds * 1e9 + LastTick.
	days := nanoseconds // Time nanosecondsInDay.
	remainingNanoseconds := nanoseconds \\ Time nanosecondsInDay.
	remainingSeconds := remainingNanoseconds // 1e9.
	remainingNanoseconds := remainingNanoseconds \\ 1e9.

	^ self basicNew
		setJdn: `DateAndTime unixEpoch julianDayNumber` + days
		seconds: remainingSeconds 
		nano: remainingNanoseconds 
		offset: utcOffset! !

!DateAndTime class methodsFor: 'system startup & shutdown' stamp: 'jmv 16/Dec/2025 13:57:15'!
initClassCachedState

	LastTickSemaphore := Semaphore forMutualExclusion.
	LastTick := 0.
	UtcOffsetInSeconds := 0.! !

!DateAndTime class methodsFor: 'system startup & shutdown' stamp: 'jmv 16/Dec/2025 13:57:33'!
releaseClassCachedState

	LastTickSemaphore := nil.
	LastTick := nil.
	UtcOffsetInSeconds := nil.! !

!DateAndTime class reorganize!
('instance creation' julianDayNumber:seconds:nanoseconds:offset: nanosecondsTick now nowUpToSeconds unixTimeSeconds: unixTimeSeconds:milliseconds:)
('ansi protocol' clockPrecision year:day:hour:minute:second: year:day:hour:minute:second:offset: year:month:day:hour:minute:second: year:month:day:hour:minute:second:offset:)
('squeak protocol' date:time: epoch fromString: julianDayNumber: localOffset midnight new noon readFrom: today tomorrow year:day: year:month:day: year:month:day:hour:minute: year:month:day:hour:minute:second:nanoSecond:offset: yesterday)
('smalltalk-80' fromSeconds:)
('initialization' initialize)
('system startup & shutdown' initClassCachedState releaseClassCachedState)
('constants' unixEpoch)
('primitive access' updateLastTickAndOffset)
!

