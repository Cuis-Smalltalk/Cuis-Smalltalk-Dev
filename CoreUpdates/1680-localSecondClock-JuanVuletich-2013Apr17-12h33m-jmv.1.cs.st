'From Cuis 4.1 of 12 December 2012 [latest update: #1679] on 17 April 2013 at 12:33:35 pm'!

!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 11:56'!
localSecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	This is in local time, i.e. the time the system shows to the user.
	Answer is (at least usually) a LargePositiveInteger.	

	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time localSecondClock } print

	"
	^self primSecondsClock! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 11:56'!
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
			seconds: Time localSecondClock) days ~= DaysSinceEpoch ]) ifTrue: [
		self initializeOffsets.
		^ self milliSecondsSinceMidnight ].
	^ msm.! !


!Debugger methodsFor: 'notifier menu' stamp: 'jmv 4/17/2013 11:56'!
storeLog
	| logFileName |
	logFileName _ Preferences debugLogTimestamp
		ifTrue: ['CuisDebug-' , Time localSecondClock printString , '.log']
		ifFalse: ['CuisDebug.log'].
	Smalltalk logError: labelString printString inContext: contextStackTop to: logFileName
! !


!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 4/17/2013 11:56'!
markRecentlyUsed
	timestamp := Time localSecondClock! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 11:56'!
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


!Utilities class methodsFor: 'identification' stamp: 'jmv 4/17/2013 11:57'!
dateTimeSuffix
	"Answer a string which indicates the date and time, intended for use in building fileout filenames, etc."

	"Utilities dateTimeSuffix"
	^ Preferences twentyFourHourFileStamps
		ifFalse:
			[self monthDayTimeStringFrom: Time localSecondClock]
		ifTrue:
			[self monthDayTime24StringFrom: Time localSecondClock]! !


!ZipStringMember methodsFor: 'initialization' stamp: 'jmv 4/17/2013 11:57'!
contents: aString
	contents _ aString.
	compressedSize _ uncompressedSize _ aString size.
	"set the file date to now"
	self setLastModFileDateTimeFrom: Time localSecondClock! !

