'From Cuis 5.0 [latest update: #4969] on 9 November 2021 at 4:47:58 pm'!

!Time class methodsFor: 'general inquiries' stamp: 'jmv 11/9/2021 16:21:57'!
localMicrosecondClock
	"Answer the number of microseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of microseconds since it was that time in this time zone.
	Answer might be a LargePositiveInteger (very likely on 32-bit images, very unlikely on 64-bit images)."

	^self primLocalMicrosecondClock! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 11/9/2021 16:21:30'!
localSecondClock
	"Answer the number of seconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of microseconds since it was that time in this time zone."

	^self primLocalMicrosecondClock // 1000000! !

!Time class methodsFor: 'primitives' stamp: 'jmv 11/9/2021 16:12:41'!
primMillisecondClock
	"Primitive. Answer the number of milliseconds since the millisecond clock
	was last reset or rolled over. No sync to any system clock.
	Implemented by all major platforms.
	Essential. See Object documentation whatIsAPrimitive. 
	
	Time primMillisecondClock
	Time primMillisecondClock / 1000 / 60.0

	Range is  from zero to 16r1FFFFFFF.
	The VM defines MillisecondClockMask as 16r1FFFFFFF

	Overflows usually every six days.
	"
"Not really a clock, but a timer or ticker"

	<primitive: 135>
	self primitiveFailed! !

