'From Cuis 4.2 of 25 July 2013 [latest update: #1987] on 18 May 2014 at 4:25:07.574292 pm'!

!Time class methodsFor: 'primitives' stamp: 'jmv 5/18/2014 15:35'!
primLocalSecondsClock
	"Answer the number of seconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of seconds since it was that time in this time zone.
	Answer is a 32-bit unsigned number, and (at least usually) a LargePositiveInteger.
	Note: This is in local time, i.e. the time the system shows to the user.
	Essential. See Object documentation whatIsAPrimitive. 

	Time primLocalSecondsClock
	Time primLocalSecondsClock / 60 / 60 / 24 / 365.25
	
	Warning: Will overflow in year 2037
	"

	<primitive: 137>
	self primitiveFailed! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 5/18/2014 16:20'!
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
	nowSecs _ Time primLocalSecondsClock.
	nowMillisecs // 1000 = nowSecs ifFalse: [
		delay _ Delay forMilliseconds: 1.
		lastSecond _ Time primLocalSecondsClock.
		[ lastSecond = Time primLocalSecondsClock ] whileTrue: [ delay wait ].
		millisecsSinceStartupOrRollover _ Time primMillisecondClock.
 		nowSecs _ lastSecond + 1.
		MillisecondClockOffset _ nowSecs * 1000 - millisecsSinceStartupOrRollover ].

	nowMillisecs _ MillisecondClockOffset + millisecsSinceStartupOrRollover.
	^nowMillisecs! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 5/18/2014 15:51'!
localSecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	This is in local time, i.e. the time the system shows to the user.
	Answer is (at least usually) a LargePositiveInteger.	

	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time localSecondClock } print
	"
	^self localMillisecondClock // 1000! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 5/18/2014 15:58'!
millisecondsToRun: timedBlock 
	"Answer the number of milliseconds timedBlock takes to return its value."

	| initialMilliseconds |
	initialMilliseconds _ self localMillisecondClock.
	timedBlock value.
	^self localMillisecondClock - initialMilliseconds! !

!Time class methodsFor: 'primitives' stamp: 'jmv 5/18/2014 15:27'!
primHighResClock	
	"Primitive. Answer the value of the high resolution clock if this computer has one.
	Usually, this should be the highest resolution value available, for example on Intel
	it will be the value of the time stamp counter register.
	Answer is (at least usually) a LargePositiveInteger.
	Implemented on Cog, but not in standard interpreter VMs."
"
	Time primHighResClock
	On Cog on OS-X, it seems to give sub nano second ticks!!

	Run a 100 msec busy loop to figure out the ticks per msec

	| t0 ticksPerMSec |
	t0 := Time millisecondClockValue + 2.
	[Time millisecondClockValue >= t0] whileFalse.
	ticksPerMSec := Time primHighResClock.
	[Time millisecondClockValue >= (t0 + 100)] whileFalse.
	ticksPerMSec := (Time primHighResClock - ticksPerMSec) 
			// (Time millisecondClockValue - t0).
	ticksPerMSec
"
"Not really a clock, but a timer or ticker"
	<primitive: 'primitiveHighResClock'>
	^0! !

!Time class methodsFor: 'primitives' stamp: 'jmv 5/18/2014 15:31'!
primLocalMicrosecondClock
	"Answer the number of microseconds since the local time zone Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in local time.
	Answer is (at least usually) a  LargePositiveInteger
	Cog VMs implement this. Interpreters might not."
	"
	Time primLocalMicrosecondClock
	Time primLocalMicrosecondClock/ 1000 / 1000 / 60 / 60 / 24 / 365.25
	"
	<primitive: 241>
	^nil! !

!Time class methodsFor: 'primitives' stamp: 'jmv 5/18/2014 15:32'!
primMillisecondClock
	"Primitive. Answer the number of milliseconds since the millisecond clock
	was last reset or rolled over. No sync to any system clock.
	Implemented by all major platforms.
	Essential. See Object documentation whatIsAPrimitive. 
	
	Time primMillisecondClock
	Time primMillisecondClock / 1000 / 60.0

	Time primMillisecondClockMask/ 1000 / 60 / 60 / 24.0
	
	Overflows usually every six days.
	Still used in #localMillisecondClock if the VM doesn't implement
		Time primLocalMicrosecondClock
	"
"Not really a clock, but a timer or ticker"

	<primitive: 135>
	self primitiveFailed! !

!Time class methodsFor: 'primitives' stamp: 'jmv 5/18/2014 15:36'!
primUtcMicrosecondClock
	"Answer the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in UTC time.
	Answer is (at least usually) a  LargePositiveInteger
	Cog VMs implement this. Interpreters might not."
	"
	Time primUtcMicrosecondClock
	Time primUtcMicrosecondClock/ 1000 / 1000 / 60 / 60 / 24 / 365.25
	"
	<primitive: 240>
	^nil! !

!Time class methodsFor: 'primitives' stamp: 'jmv 5/18/2014 15:43'!
primUtcWithOffset
	"Answer a two element array.

	First element is the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in UTC time.
	It is (at least usually) a  LargePositiveInteger.
	
	Second element is the current seconds offset from GMT in the local time zone.
	
	Warning 1: When this primitive was discussed in VM-Dev, first element was said to be in Unix epoch (1/1/1970), but it seems it is in Smalltalk epoch. (This needs to be checked on various platforms).
	
	Warning 2: I (jmv) can not make sense of the second element. I get 62581760, that is almost two years.

	Warning 3: Cog VMs implement this. Interpreters might not.
	
	Because of this, Cuis is not currently using this primitive.
	"
	"
	Time primUtcWithOffset
	Time primUtcWithOffset first / 1000 / 1000 / 60 / 60 / 24 / 365.25
	Time primUtcWithOffset second / 60 / 60.0
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


!Random methodsFor: 'initialization' stamp: 'jmv 5/18/2014 16:15'!
initialize
	" Set a reasonable Park-Miller starting seed "
	[
		seed := (Time localMillisecondClock bitAnd: 16r3FFFFFFF) bitXor: self hash.
		seed = 0] whileTrue. 	"Try again if ever get a seed = 0"

	a := 16r000041A7 asFloat.    " magic constant =      16807 "
	m := 16r7FFFFFFF asFloat.    " magic constant = 2147483647 "
	q := (m quo: a) asFloat.
	r  := (m \\ a) asFloat.
! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 5/18/2014 15:50'!
monthDayTimeStringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the form:
		ddMMMhhmmPP	  where:
							dd is a two-digit day-of-month,
							MMM is the alpha month abbreviation,
							hhmm is the time,
							PP is either am or pm

          Utilities monthDayTimeStringFrom: Time localSecondClock
	"

	aDate _ Date fromSeconds: aSecondCount.
	aTime _ Time fromSeconds: aSecondCount \\ Time secondsInDay.
	^ String streamContents: [ :strm |
		aDate dayOfMonth printOn: strm integerDigits: 2 fractionDigits: 0.
		strm nextPutAll: (aDate monthName copyFrom: 1 to: 3).
		aTime hour \\ 12 printOn: strm integerDigits: 2 fractionDigits: 0.
		aTime minute printOn: strm integerDigits: 2 fractionDigits: 0.
		strm nextPutAll: (aTime hour > 12 ifTrue: ['pm'] ifFalse: ['am'])
		]! !

!methodRemoval: Time class #benchmarkMillisecondClock!
Time class removeSelector: #benchmarkMillisecondClock!
!methodRemoval: Time class #millisecondClockMask!
Time class removeSelector: #millisecondClockMask!
!methodRemoval: Time class #milliseconds:since:!
Time class removeSelector: #milliseconds:since:!
!methodRemoval: Time class #millisecondsSince:!
Time class removeSelector: #millisecondsSince:!
!methodRemoval: Time class #primMicrosecondClock!
Time class removeSelector: #primMicrosecondClock!
!methodRemoval: Time class #primSecondsClock!
Time class removeSelector: #primSecondsClock!
