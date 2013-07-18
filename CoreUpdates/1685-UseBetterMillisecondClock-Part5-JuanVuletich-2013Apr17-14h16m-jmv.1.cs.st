'From Cuis 4.1 of 12 December 2012 [latest update: #1684] on 17 April 2013 at 2:19:14.132429 pm'!
!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos '
	classVariableNames: 'DaysSinceEpoch LastMilliSeconds LastTick LastTickSemaphore LocalTimeZone MilliSecondOffset OffsetsAreValid '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!

!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 13:26'!
localMicrosecondClock
	"Answer the number of microseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of microseconds since it was that time in this time zone.
	Answer is (at least usually) a LargePositiveInteger.
	
	{ Time localMicrosecondClock . Time localMicrosecondClock // 1000000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.
	No dependency on MillisecondClockMask.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	| |
	
	"If our VM supports the new primitive, just use it."
	Time primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs ].
	
	"Otherwise we'll have just millisecond precision"
	^self localMillisecondClock * 1000! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 13:34'!
microsecondsInDay
	^86400000000! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 13:43'!
nanosecondsInDay
	^86400000000000! !


!DateAndTime class methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 13:53'!
now
	"Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as TimeStamps"

	| days nanoseconds remainingNanoseconds remainingSeconds |
	nanoseconds _ Time localMicrosecondClock * 1000.
	(LastTick < nanoseconds)
		ifTrue: [LastTick _ nanoseconds]
		ifFalse: [
			LastTickSemaphore critical: [
				LastTick _  LastTick + 1.
				nanoseconds _ LastTick ]].

	days _ nanoseconds // Time nanosecondsInDay.
	remainingNanoseconds _ nanoseconds \\ Time nanosecondsInDay.
	remainingSeconds _ remainingNanoseconds // 1000000000.
	remainingNanoseconds _ remainingNanoseconds \\ 1000000000.
	^ self basicNew
		setJdn: Time squeakEpoch + days 
		seconds: remainingSeconds 
		nano: remainingNanoseconds 
		offset: self localOffset! !

!DateAndTime class methodsFor: 'initialize-release' stamp: 'jmv 4/17/2013 14:18'!
initialize
	"
	self initialize
	"
	LastTickSemaphore := Semaphore forMutualExclusion.
	LastTick := 0.
	Smalltalk addToStartUpList: self.
	self startUp: true! !

!DateAndTime class methodsFor: 'initialize-release' stamp: 'jmv 4/17/2013 13:50'!
startUp: resuming
	resuming ifFalse: [ ^ self ].
	Time initializeMillisecondClockMask! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 13:55'!
localMillisecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	Answer is (at least usually) a LargePositiveInteger.
	
	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.
	No dependency on MillisecondClockMask.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	| millisecsSinceStartupOrRollover nowMillisecs nowSecs delay lastSecond |
	
	"If our VM supports the new primitive, just use it."
	Time primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs // 1000 ].

	"Otherwise use millisecond clock and offset from second clock"
	MillisecondClockOffset ifNil: [ MillisecondClockOffset _ 0 ].	"Fix it below."
	millisecsSinceStartupOrRollover _ Time primMillisecondClock.
	nowMillisecs _ millisecsSinceStartupOrRollover + MillisecondClockOffset.

	"Do expensive resync (delay 1 second) only on primMillisecondClock rollover, or if for whatever reason offset is wrong."
	nowSecs _ Time primSecondsClock.
	nowMillisecs // 1000 = nowSecs ifFalse: [
		delay _ Delay forMilliseconds: 1.
		lastSecond _ Time primSecondsClock.
		[ lastSecond = Time primSecondsClock ] whileTrue: [ delay wait ].
		millisecsSinceStartupOrRollover _ Time primMillisecondClock.
 		nowSecs _ lastSecond + 1.
		MillisecondClockOffset _ nowSecs * 1000 - millisecsSinceStartupOrRollover ].

	nowMillisecs _ MillisecondClockOffset + millisecsSinceStartupOrRollover.
	^nowMillisecs! !

!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 13:59'!
dateAndTimeNow
	"Answer a two-element Array of (Date today, Time now)."

	| dateAndTime |
	dateAndTime _ DateAndTime now.
	^{ dateAndTime date . dateAndTime time }! !

!Time class methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 14:06'!
now
	"Answer a Time representing the time right now - this is a 24 hour clock.
	Precision is microsecond if the VM supports it, millisecond otherwise"

	| microseconds |
	microseconds _ self localMicrosecondClock \\ self microsecondsInDay.
	^ self seconds:  (microseconds // 1000000) nanoSeconds: (microseconds \\ 1000000) * 1000! !

!methodRemoval: Time class #secondsWhenClockTicks!
Time class removeSelector: #secondsWhenClockTicks!
!methodRemoval: DateAndTime class #initializeOffsets!
DateAndTime class removeSelector: #initializeOffsets!
!methodRemoval: DateAndTime class #milliSecondsSinceMidnight!
DateAndTime class removeSelector: #milliSecondsSinceMidnight!
!methodRemoval: DateAndTime class #todayAtMilliSeconds:!
DateAndTime class removeSelector: #todayAtMilliSeconds:!
!methodRemoval: DateAndTime class #todayAtNanoSeconds:!
DateAndTime class removeSelector: #todayAtNanoSeconds:!
!methodRemoval: DateAndTime class #waitForOffsets!
DateAndTime class removeSelector: #waitForOffsets!
DateAndTime initialize!

!DateAndTime class reorganize!
('ansi protocol' clockPrecision now year:day:hour:minute:second: year:day:hour:minute:second:offset: year:month:day:hour:minute:second: year:month:day:hour:minute:second:offset:)
('squeak protocol' date:time: epoch fromString: julianDayNumber: localOffset localTimeZone localTimeZone: midnight new noon readFrom: today tomorrow year:day: year:month:day: year:month:day:hour:minute: year:month:day:hour:minute:second:nanoSecond:offset: yesterday)
('smalltalk-80' fromSeconds:)
('initialize-release' initialize startUp:)
!

!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos'
	classVariableNames: 'LastTick LastTickSemaphore LocalTimeZone'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
