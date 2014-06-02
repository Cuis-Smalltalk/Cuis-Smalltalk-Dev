'From Cuis 4.2 of 25 July 2013 [latest update: #1996] on 25 May 2014 at 9:43:09.583987 pm'!

!Time class methodsFor: 'primitives' stamp: 'jmv 5/25/2014 21:41'!
primHighResClock	
	"Primitive. Answer the value of the high resolution clock if this computer has one.
	Usually, this should be the highest resolution value available, for example on Intel
	it will be the value of the time stamp counter register.
	Answer is (at least usually) a LargePositiveInteger.
	Implemented on Cog, but not in standard interpreter VMs."
"
	Time primHighResClock
	On Cog on OS-X and Windows, it seems to give sub nano second ticks!!

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

