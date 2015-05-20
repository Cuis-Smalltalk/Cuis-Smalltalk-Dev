'From Cuis 4.2 of 25 July 2013 [latest update: #2327] on 20 May 2015 at 8:44:06.967987 am'!

!ByteArray class methodsFor: 'instance creation' stamp: 'jmv 5/20/2015 08:40'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection.
	Argument can also be a LargeInteger!!"
	| newArray |
	newArray _ self new: aCollection size.
	1 to: aCollection size do: [:i | newArray at: i put: (aCollection at: i)].
	^ newArray

"
	ByteArray newFrom: 1000 factorial
	
	| ba li |
	ba _ ByteArray newFrom: 1000 factorial.
	li _ LargePositiveInteger new: ba size.
	li replaceFrom: 1 to: ba size with: ba startingAt: 1.
	li / 999 factorial
"! !


!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 5/19/2015 21:53'!
bench
	"See how many times I can value in 5 seconds.  I'll answer a meaningful description."

	| startTime endTime count roundTo3Digits |
	roundTo3Digits := [:num |
		| rounded lowDigit |
		rounded := (num * 1000) rounded. "round to 1/1000"
		lowDigit := (rounded numberOfDigitsInBase: 10) - 3. "keep only first 3 digits"
		rounded := rounded roundTo:(10 raisedTo: lowDigit).
		(lowDigit >= 3 or: [rounded \\ 1000 = 0]) "display fractional part only when needed"
			ifTrue: [(rounded // 1000) printStringWithCommas]
			ifFalse: [(rounded / 1000.0) printString]].
	count := 0.
	endTime := Time localMillisecondClock + 5000.
	startTime := Time localMillisecondClock.
	[ Time localMillisecondClock > endTime ] whileFalse: [ self value.  count := count + 1 ].
	endTime := Time localMillisecondClock.
	^count = 1
		ifTrue: [ (roundTo3Digits value: (endTime - startTime) / 1000) , ' seconds.' ]
		ifFalse:
			[ (roundTo3Digits value: (count * 1000) / (endTime - startTime)) , ' per second.' ]! !


!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 5/19/2015 23:45'!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize _ stop - start + 1 max: 0.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 5/19/2015 21:49'!
localMicrosecondClock
	"Answer the number of microseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of microseconds since it was that time in this time zone.
	Answer is (at least usually) a LargePositiveInteger.
	
	{ Time localMicrosecondClock . Time localMicrosecondClock // 1000000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.

	Note 2: This is in local time, i.e. the time the system shows to the user. UTC would be better,
	but older VMs don't know about the current time zone"
	
	"If our VM supports the new primitive, just use it."
	Time primLocalMicrosecondClock ifNotNil: [ :microSecs | ^microSecs ].
	
	"Otherwise we'll have just millisecond precision"
	^self localMillisecondClock * 1000! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 5/19/2015 21:49'!
localMillisecondClock
	"Answer the number of milliseconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of milliseconds since it was that time in this time zone.
	Answer is (at least usually) a LargePositiveInteger.
	
	{ Time localMillisecondClock . Time localMillisecondClock // 1000 . Time localSecondClock } print
	
	Note 1: Never rolls over. Can be used with confidence. Joins primMillisecondClock
	rollover control and sync with seconds clock. Uses existing offset if any, and validates result.
	Resynchs if needed. Resynch is very robust. No problems if clocks go out of synch for unknown reasons.

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

!Time class methodsFor: 'primitives' stamp: 'jmv 5/19/2015 21:49'!
primMillisecondClock
	"Primitive. Answer the number of milliseconds since the millisecond clock
	was last reset or rolled over. No sync to any system clock.
	Implemented by all major platforms.
	Essential. See Object documentation whatIsAPrimitive. 
	
	Time primMillisecondClock
	Time primMillisecondClock / 1000 / 60.0
	
	Overflows usually every six days.
	Still used in #localMillisecondClock if the VM doesn't implement
		Time primLocalMicrosecondClock
	"
"Not really a clock, but a timer or ticker"

	<primitive: 135>
	self primitiveFailed! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 5/19/2015 22:02'!
classFromPattern: pattern withCaption: aCaption
	"If there is a class whose name exactly given by pattern, return it.
	If there is only one class in the system whose name matches pattern, return it.
	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.
	This method ignores tab, space, & newline characters in the pattern"

	| toMatch potentialClassNames classNames exactMatch index |
	(toMatch _  pattern copyWithoutAll:
			{Character space.  Character newLineCharacter.  Character tab})
		isEmpty ifTrue: [^ nil].
	Symbol hasInterned: toMatch ifTrue:
		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:
			[:maybeClass | (maybeClass isKindOf: Class) ifTrue: [^ maybeClass]]].

	toMatch _ (toMatch copyWithout: $.) asLowercase.
	potentialClassNames _ Smalltalk classNames asOrderedCollection.
	classNames _ pattern last = $. 
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ nil].
	exactMatch _ classNames detect: [:each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].
	index = 0 ifTrue: [^ nil].
	^ Smalltalk at: (classNames at: index) asSymbol

"
	Utilities classFromPattern: 'CharRecog' withCaption: 'xx'
	Utilities classFromPattern: 'rRecog' withCaption: 'xx'
	Utilities classFromPattern: 'znak' withCaption: 'xx'
	Utilities classFromPattern: 'orph' withCaption: 'xx'
"
! !

