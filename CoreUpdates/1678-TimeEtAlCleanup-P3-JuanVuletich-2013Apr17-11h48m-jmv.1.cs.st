'From Cuis 4.1 of 12 December 2012 [latest update: #1677] on 17 April 2013 at 11:50:02 am'!

!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 08:56'!
milliSecondsSinceMidnight
	| msm msClock |

	"This is usually only during system startup..."
	self waitForOffsets.

	msClock _ Time millisecondClockValue.
	msClock < LastMilliSeconds ifTrue: [ "rolled over"
		MilliSecondOffset _ MilliSecondOffset + Time millisecondClockMask + 1 ].
	LastMilliSeconds _ msClock.
	[
	msm _ msClock + MilliSecondOffset.
	msm >= Time millisecondsInDay ] whileTrue: [
		"next day"
		LastTick _ -1.
		DaysSinceEpoch _ DaysSinceEpoch + 1.
		MilliSecondOffset _ MilliSecondOffset - Time millisecondsInDay ].
	"day rolled over sanity check"
	(LastTick = -1 and: [
		(Duration
			days: Time squeakEpoch
			hours: 0
			minutes: 0
			seconds: Time primSecondsClock) days ~= DaysSinceEpoch ]) ifTrue: [
		self initializeOffsets.
		^ self milliSecondsSinceMidnight ].
	^ msm.! !


!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 08:45'!
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
	<primitive: 'primitiveHighResClock'>
	^0! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 11:41'!
primMillisecondClock
	"Primitive. Answer the number of milliseconds since the millisecond clock
	was last reset or rolled over. No sync to any system clock.
	Implemented by all major platforms.
	Essential. See Object documentation whatIsAPrimitive. 
	
	Time primMillisecondClock
	"

	<primitive: 135>
	self primitiveFailed! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 08:47'!
primMillisecondClockMask
	"Answer the mask value used for millisecond clock rollover in the
	virtual machine, or nil if the VM cannot support the request.
	Time primMillisecondClockMask highBit 29 (29 bits on Cog on OS-X)
	"

	<primitive: 'primitiveMillisecondClockMask'>
	^ nil! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 08:48'!
primSecondsClock
	"Answer the number of seconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of seconds since it was that time in this time zone.
	Answer is a 32-bit unsigned number, and (at least usually) a LargePositiveInteger.
	Note: This is in local time, i.e. the time the system shows to the user.
	Essential. See Object documentation whatIsAPrimitive. 

	Time primSecondsClock
	"

	<primitive: 137>
	self primitiveFailed! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 08:49'!
primUtcWithOffset
	"Answer a two element array.

	First element is the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in UTC time.
	It is (at least usually) a  LargePositiveInteger.
	
	Second element is the current seconds offset from GMT in the local time zone.
	
	Warning 1: When this primitive was discussed in VM-Dev, first element was said to be in Unix epoch (1/1/1970), but it seems it is in Smalltalk epoch.
	
	Warning 2: I (jmv) can not make sense of the second element. I get 62581760, that is almost two years.

	Warning 3: Cog VMs implement this. Interpreters might not.
	
	Because of this, Cuis is not currently using this primitive.
	"
	"
	Time primUtcWithOffset
	"
	<primitive: 'primitiveUtcWithOffset'>
	^nil! !

!methodRemoval: BlockContext #bench!
BlockContext removeSelector: #bench!
!methodRemoval: BlockClosure #bench!
BlockClosure removeSelector: #bench!
