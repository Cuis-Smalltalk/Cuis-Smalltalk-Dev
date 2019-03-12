'From Cuis 5.0 of 7 November 2016 [latest update: #3655] on 12 March 2019 at 2:14:59 pm'!

!Date methodsFor: 'squeak protocol' stamp: 'jmv 3/12/2019 13:50:59'!
> aDate

	self assert: aDate class == Date.
	^ self start > aDate start! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 3/12/2019 14:02:51'!
localMicrosecondClock
	"Answer the number of microseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of microseconds since it was that time in this time zone.
	Answer might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	
	{ Time localMicrosecondClock . Time localMicrosecondClock // 1000000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	"If our VM supports the new primitive, just use it."
	self primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs ].
	
	"Otherwise we'll have just millisecond precision"
	^self localMillisecondClock * 1000! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 3/12/2019 14:02:55'!
localMillisecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	Answer might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	
	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	| millisecsSinceStartupOrRollover nowMillisecs nowSecs delay lastSecond |
	
	"If our VM supports the new primitive, just use it."
	self primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs // 1000 ].

	"Otherwise use millisecond clock and offset from second clock"
	MillisecondClockOffset ifNil: [ MillisecondClockOffset _ 0 ].	"Fix it below."
	millisecsSinceStartupOrRollover _ self primMillisecondClock.
	nowMillisecs _ millisecsSinceStartupOrRollover + MillisecondClockOffset.

	"Do expensive resync (delay 1 second) only on primMillisecondClock rollover, or if for whatever reason offset is wrong."
	nowSecs _ self primLocalSecondsClock.
	nowMillisecs // 1000 = nowSecs ifFalse: [
		delay _ Delay forMilliseconds: 1.
		lastSecond _ self primLocalSecondsClock.
		[ lastSecond = self primLocalSecondsClock ] whileTrue: [ delay wait ].
		millisecsSinceStartupOrRollover _ self primMillisecondClock.
 		nowSecs _ lastSecond + 1.
		MillisecondClockOffset _ nowSecs * 1000 - millisecsSinceStartupOrRollover ].

	nowMillisecs _ MillisecondClockOffset + millisecsSinceStartupOrRollover.
	^nowMillisecs! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 3/12/2019 14:03:05'!
localSecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	This is in local time, i.e. the time the system shows to the user.
	Answer might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).

	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time localSecondClock } print
	"
	^self localMillisecondClock // 1000! !

!Time class methodsFor: 'primitives' stamp: 'jmv 3/12/2019 14:03:29'!
primHighResClock	
	"Primitive. Answer the value of the high resolution clock if this computer has one.
	Usually, this should be the highest resolution value available, for example on Intel
	it will be the value of the time stamp counter register.
	Answer might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	Implemented on Cog, but not in standard interpreter VMs."
	"
	Time primHighResClock
	On Cog on Linux, OS-X and Windows, this gives sub nano second ticks!!

	Time highResTimerTicksPerMillisecond
	"
	"Not really a clock, but a timer or ticker"

	<primitive: 'primitiveHighResClock'>
	^0! !

!Time class methodsFor: 'primitives' stamp: 'jmv 3/12/2019 14:03:33'!
primLocalMicrosecondClock
	"Answer the number of microseconds since the local time zone Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in local time.
	Answer might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	Cog VMs implement this. Interpreters might not."
	"
	Time primLocalMicrosecondClock
	Time primLocalMicrosecondClock/ 1000 / 1000 / 60 / 60 / 24 / 365.25
	"
	<primitive: 241>
	^nil! !

!Time class methodsFor: 'primitives' stamp: 'jmv 3/12/2019 14:05:35'!
primLocalSecondsClock
	"Answer the number of seconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of seconds since it was that time in this time zone.
	Answer is a 32-bit unsigned number.
	Answer might be a LargePositiveInteger on 32-bit images.
	Note: This is in local time, i.e. the time the system shows to the user.
	Essential. See Object documentation whatIsAPrimitive. 

	Time primLocalSecondsClock
	Time primLocalSecondsClock / 60 / 60 / 24 / 365.25
	
	Warning: Will overflow in year 2037
	"

	<primitive: 137>
	self primitiveFailed! !

!Time class methodsFor: 'primitives' stamp: 'jmv 3/12/2019 14:05:48'!
primUtcMicrosecondClock
	"Answer the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, the start of the 20th century, in UTC time.
	 The value is derived from the Posix epoch with a constant offset corresponding to elapsed microseconds
	 between the two epochs according to RFC 868.
	Answer might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	Cog VMs implement this. Interpreters might not."
	"
	Time primUtcMicrosecondClock
	Time primUtcMicrosecondClock/ 1000 / 1000 / 60 / 60 / 24 / 365.25

	(Time primUtcMicrosecondClock / 1000 / 1000 + Time primUtcWithOffset second) / 60 / 60 / 24 / 365.25
	(DateAndTime now - (DateAndTime year: 1901 month: 1 day: 1)) totalSeconds / 60 / 60 / 24 / 365.25
	"
	<primitive: 240>
	^nil! !

!Time class methodsFor: 'primitives' stamp: 'jmv 3/12/2019 14:07:31'!
primUtcWithOffset
	"Answer a two element array.

	First element is the number of microseconds since the UTC Posix (Unix) epoch, 
	i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	It might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images).
	
	Second element is the current seconds offset from GMT in the local time zone.

	Recent VMs implement this. Older Interpreters might not.
	"
	"
	Time primUtcWithOffset
	Time primUtcWithOffset first / 1000 / 1000 / 60 / 60 / 24 / 365.25
	Time primUtcWithOffset second / 60 / 60.0
		
	(Time primUtcWithOffset first / 1000 / 1000 + Time primUtcWithOffset second) / 60 / 60 / 24 / 365.25
	(DateAndTime now - (DateAndTime year: 1970 month: 1 day: 1)) totalSeconds / 60 / 60 / 24 / 365.25
	"
	<primitive: 'primitiveUtcWithOffset'>
	^nil
	
	"
	Evaluate on various platforms and record the results
{ 
	Smalltalk vmVersion .
	Smalltalk platformName .
	Smalltalk platformSubtype  .
	Smalltalk osVersion .
	Time primLocalMicrosecondClock/ 1000 / 1000 / 60 / 60 / 24 / 365.25 .
	Time primLocalSecondsClock / 60 / 60 / 24 / 365.25 . 
	Time primUtcMicrosecondClock / 1000 / 1000 / 60 / 60 / 24 / 365.25 . 
	Time primUtcWithOffset first / 1000 / 1000 / 60 / 60 / 24 / 365.25 . 
	Time primUtcWithOffset second / 60 / 60.0
}
	"! !

