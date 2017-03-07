'From Cuis 5.0 of 7 November 2016 [latest update: #3061] on 7 March 2017 at 12:20:30 pm'!

!Time class methodsFor: 'primitives' stamp: 'jmv 3/7/2017 12:16:43'!
primUtcMicrosecondClock
	"Answer the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, the start of the 20th century, in UTC time.
	 The value is derived from the Posix epoch with a constant offset corresponding to elapsed microseconds
	 between the two epochs according to RFC 868.
	Answer is (at least usually) a  LargePositiveInteger
	Cog VMs implement this. Interpreters might not."
	"
	Time primUtcMicrosecondClock
	Time primUtcMicrosecondClock/ 1000 / 1000 / 60 / 60 / 24 / 365.25

	(Time primUtcMicrosecondClock / 1000 / 1000 + Time primUtcWithOffset second) / 60 / 60 / 24 / 365.25
	(DateAndTime now - (DateAndTime year: 1901 month: 1 day: 1)) totalSeconds / 60 / 60 / 24 / 365.25
	"
	<primitive: 240>
	^nil! !


!Delay class methodsFor: 'primitives' stamp: 'jmv 3/7/2017 12:19:39'!
primSignal: aSemaphore atUTCMicroseconds: anInteger
	"Signal the semaphore when the UTC microsecond clock reaches the value of the second argument.
	 Fail if the first argument is neither a Semaphore nor nil.
	Fail if the second argument is not an integer (either SmallInteger or LargePositiveInteger).
	See #primUtcMicrosecondClock
	 Essential. See Object documentation whatIsAPrimitive."
	<primitive: 242>
	^self primitiveFailed! !

