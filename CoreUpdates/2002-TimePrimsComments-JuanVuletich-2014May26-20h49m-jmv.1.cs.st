'From Cuis 4.2 of 25 July 2013 [latest update: #2002] on 26 May 2014 at 8:58:37.82476 pm'!

!Time class methodsFor: 'primitives' stamp: 'jmv 5/26/2014 20:58'!
primUtcMicrosecondClock
	"Answer the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in UTC time.
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

!Time class methodsFor: 'primitives' stamp: 'jmv 5/26/2014 20:58'!
primUtcWithOffset
	"Answer a two element array.

	First element is the number of microseconds since the UTC Posix (Unix) epoch, 
	i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	It is (at least usually) a  LargePositiveInteger.
	
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

