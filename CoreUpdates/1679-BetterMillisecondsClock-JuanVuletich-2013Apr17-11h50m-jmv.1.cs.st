'From Cuis 4.1 of 12 December 2012 [latest update: #1677] on 17 April 2013 at 11:51:25 am'!
!classDefinition: #Time category: #'Kernel-Chronology'!
Magnitude subclass: #Time
	instanceVariableNames: 'seconds nanos '
	classVariableNames: 'MillisecondClockMask MillisecondClockOffset '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!

!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 11:44'!
localMillisecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	Answer is (at least usually) a LargePositiveInteger.
	
	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time primSecondsClock }
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.
	No dependency on MillisecondClockMask.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	| millisecsSinceStartupOrRollover nowMillisecs nowSecs delay lastSecond |
	
	"If our VM supports the new primitive, just use it."
	Time primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs // 1000 ].
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

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 11:13'!
primLocalMicrosecondClock
	"Answer the number of microseconds since the local time zone Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in local time.
	Answer is (at least usually) a  LargePositiveInteger
	Cog VMs implement this. Interpreters might not."
	"
	Time primLocalMicrosecondClock
	"
	<primitive: 241>
	^nil! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 11:05'!
millisecondClockValue
	"Answer the number of milliseconds since the millisecond clock was last reset or rolled over.
	Answer 0 if the primitive fails.
	
	Note: Prefer #localMillisecondClock, that doesn't roll over."

	^ self primMillisecondClock! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 11:43'!
primMicrosecondClock
	"Answer the number of microseconds since the microsecond clock
	was last reset or rolled over. Answer zero if the primitive fails.
	Note: Primitive fails on Cog. It works 'Squeak 4.2.5beta1U.app' on OS-X and on 'Squeak4.10.2-2612' on Windows.
	Reset at startup. Rolls over at?????"
	"
	Warning. Running 'Squeak 4.2.5beta1U.app' on OS-X, this primitive gives reasonable results initially after startup:
	
	Evaluate this seconds after staring the image:
	{ Time primMicrosecondClock hex . Time primMicrosecondClock / 1000.0/1000/60 . Time primMillisecondClock / 1000.0 / 60 } 
	 #('16rE78CA5' 0.25291395 0.2529)
	So far, ok.
	
	Now, wait 40 minutes:
	#('16rFFFFFFFF9000FB02' 3.074457345305095e11 40.26638333333334)
	It looks like a 32 bit number was 'sign extended'  into 64 bits or something.
	
	It looks like we'd better use Cog and #primLocalMicrosecondClock and #primUtcMicrosecondClock if we want microsecond precision.
	In any case, it looks like we could implement #localMicrosecondClock very much like #localMillisecondClock
	"
	
	"
	Time primMicrosecondClock
	"

	<primitive: 'primitiveMicrosecondClock'>
	^ 0! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 11:13'!
primUtcMicrosecondClock
	"Answer the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in UTC time.
	Answer is (at least usually) a  LargePositiveInteger
	Cog VMs implement this. Interpreters might not."
	"
	Time primUtcMicrosecondClock
	"
	<primitive: 240>
	^nil! !

!Time class methodsFor: 'private' stamp: 'jmv 4/17/2013 11:04'!
currentUtcOffset
	"Find out the current offset from UTC by calling both a primitive that answers UTC time and one that answers local time.
	As there is a slight difference in time between both primitive calls, round the answer to whole minutes.
	Answer a Duration.
	Works only on VMs that include the primitives (like Cog)"
	"
	Time currentUtcOffset
	"
	| utcMicroseconds locaMicroseconds |
	utcMicroseconds _ self primUtcMicrosecondClock.
	locaMicroseconds _ self primLocalMicrosecondClock.
	^(locaMicroseconds - utcMicroseconds / 1000000.0 / 60.0) rounded minutes! !

!classDefinition: #Time category: #'Kernel-Chronology'!
Magnitude subclass: #Time
	instanceVariableNames: 'seconds nanos'
	classVariableNames: 'MillisecondClockMask MillisecondClockOffset'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
