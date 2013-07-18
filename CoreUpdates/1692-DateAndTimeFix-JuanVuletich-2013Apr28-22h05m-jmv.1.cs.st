'From Cuis 4.1 of 12 December 2012 [latest update: #1691] on 28 April 2013 at 10:05:54.724726 pm'!

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 4/28/2013 20:51'!
asUTC

	^ offset isZero
		ifTrue: [self]
		ifFalse: [self utcOffset: 0 hours]
! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/28/2013 21:56'!
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
	
	"If our VM supports the new primitive, just use it."
	Time primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs ].
	
	"Otherwise we'll have just millisecond precision"
	^self localMillisecondClock * 1000! !

