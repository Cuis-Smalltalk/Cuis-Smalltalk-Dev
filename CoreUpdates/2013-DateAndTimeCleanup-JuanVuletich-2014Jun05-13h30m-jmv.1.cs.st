'From Cuis 4.2 of 25 July 2013 [latest update: #2012] on 5 June 2014 at 2:36:11.301712 pm'!
!classDefinition: #DateAndTimeTest category: #'Kernel-Chronology tests'!
TestCase subclass: #DateAndTimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology tests'!
!classDefinition: #TimeTest category: #'Kernel-Chronology tests'!
TestCase subclass: #TimeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology tests'!

!DateAndTime methodsFor: 'private' stamp: 'jmv 6/4/2014 09:30'!
julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset
	"secs could span more than one day. nsecs could span more than one second."
	
	| daysOverflow nanosSinceSec secsOverflow secsSinceMidnight totalSecs totalDays |
	"Add to totalSecs any whole seconds in nsecs"
	secsOverflow _ nsecs // Time nanosInSecond.
	nanosSinceSec _ nsecs - (secsOverflow * Time nanosInSecond).
	totalSecs _ secs + secsOverflow.

	"Add to totalDays any whole days in totalSecs"
	daysOverflow _ totalSecs // Time secondsInDay.
	secsSinceMidnight _ totalSecs - (daysOverflow * Time secondsInDay).
	totalDays _ day + daysOverflow.

	jdn _ totalDays.
	seconds _ secsSinceMidnight.
	nanos _ nanosSinceSec.
	offset _ utcOffset! !


!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 6/4/2014 09:30'!
julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset

	^self basicNew julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset! !


!Time class methodsFor: 'squeak protocol' stamp: 'jmv 6/4/2014 06:54'!
afterMidnight: aDuration
	"Answer a Time aDuration after midnight.
	If aDuration is longer than a day, take the day fraction."

	| nanoCount secondCount |
	secondCount _ aDuration totalSeconds rem: Time secondsInDay.
	nanoCount _ aDuration nanoSeconds.
	^ self basicNew seconds: secondCount nanoSeconds: nanoCount ! !


!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:29'!
testCreation
	"
	self new testCreation
	"
	| d |
	d _ (DateAndTime julianDayNumber: 2456814) date.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	
	d _ (DateAndTime fromSeconds: 3579379200) date.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	
	d _ DateAndTime fromSeconds: 3579431284.
	self assert: d yearNumber = 2014.
	self assert: d monthIndex = 6.
	self assert: d dayOfMonth = 5.
	self assert: d hour = 14.
	self assert: d minute = 28.
	self assert: d second = 4.
	self assert: d nanoSecond = 0! !

!DateAndTimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:22'!
testOperations
	"
	self new testOperations
	"
	| d dt |
	dt _ DateAndTime now.

	d _ 1 hours.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ 1 minutes.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ 1 seconds.
	self assert: dt < (dt + d).
	self assert: dt - d < dt.
	self assert: dt + d - d = dt.

	d _ -1 hours.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.

	d _ -1 minutes.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.

	d _ -1 seconds.
	self assert: dt < (dt - d).
	self assert: dt + d < dt.
	self assert: dt + d - d = dt.! !


!TimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:32'!
testCreation
	"
	self new testCreation
	"
	| t |
	
	t _ Time seconds: 52262.
	self assert: t hour = 14.
	self assert: t minute = 31.
	self assert: t second = 2.

	t _ Time seconds: 52262 nanoSeconds: 414712000.
	self assert: t hour = 14.
	self assert: t minute = 31.
	self assert: t second = 2.
	self assert: t nanoSecond = 414712000! !

!TimeTest methodsFor: 'tests' stamp: 'jmv 6/5/2014 14:34'!
testEqual
	"
	self new testEqual
	"
	| t1 t2 |
	
	t1 _ Time seconds: 52262.
	t2 _ Time seconds: 52262 nanoSeconds: 0.
	self assert: t1 = t2.
	t2 _ Time seconds: 52262 nanoSeconds: 1234.
	self deny: t1 = t2.
	t2 _ Time seconds: 52263 nanoSeconds: 0.
	self deny: t1 = t2.! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 6/4/2014 09:37'!
+ operand
	"operand conforms to protocol Duration"

	self assert: operand class == Duration.

	^ self class
		julianDayNumber: jdn
		seconds: seconds + operand totalSeconds
		nanoseconds: nanos + operand nanoSeconds
		offset: offset! !

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 6/4/2014 09:02'!
- operand
	"operand is a DateAndTime or a Duration"

	| lvalue rvalue |
	(operand is: #DateAndTime)
		ifTrue: [
			offset = operand offset
				ifTrue: [
					lvalue _ self.
					rvalue _ operand ]
				ifFalse: [
					lvalue _ self asUTC.
					rvalue _ operand asUTC ].
			^ Duration
 				seconds: (Time secondsInDay *(lvalue julianDayNumber - rvalue julianDayNumber)) + 
							(lvalue secondsSinceMidnight - rvalue secondsSinceMidnight)
 				nanoSeconds: lvalue nanoSecond - rvalue nanoSecond ].
	^self + operand negated! !

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 6/3/2014 18:55'!
< comparand
	"comparand is a DateAndTime."
	| lvalue rvalue |
	self assert: (comparand is: #DateAndTime).
	offset = comparand offset
		ifTrue: [
			lvalue _ self.
			rvalue _ comparand ]
		ifFalse: [
			lvalue _ self asUTC.
			rvalue _ comparand asUTC ].
	^ lvalue julianDayNumber < rvalue julianDayNumber
		or: [
			lvalue julianDayNumber > rvalue julianDayNumber
				ifTrue: [ false ]
				ifFalse: [
					lvalue secondsSinceMidnight < rvalue secondsSinceMidnight
						or: [
							lvalue secondsSinceMidnight > rvalue secondsSinceMidnight
								ifTrue: [ false ]
								ifFalse: [ lvalue nanoSecond < rvalue nanoSecond ]]]]! !

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 6/4/2014 09:02'!
= comparand
	"comparand is a DateAndTime."
	| lvalue rvalue |

	"Any object is equal to itself"
	self == comparand ifTrue: [ ^ true ].

	self class == comparand class ifFalse: [ ^false ].

	offset = comparand offset
		ifTrue: [
			lvalue _ self.
			rvalue _ comparand ]
		ifFalse: [
			lvalue _ self asUTC.
			rvalue _ comparand asUTC ].
	^ lvalue julianDayNumber = rvalue julianDayNumber
		and: [ lvalue secondsSinceMidnight = rvalue secondsSinceMidnight
			and: [ lvalue nanoSecond = rvalue nanoSecond ] ]! !

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 6/3/2014 20:01'!
hash

	^(jdn bitXor: seconds) bitXor: nanos! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 6/4/2014 09:38'!
utcOffset: anOffset

	"Answer a <DateAndTime> equivalent to the receiver but offset from UTC by anOffset"

	| diff |
	self assert: anOffset class == Duration.

	diff _ anOffset - self offset.
	^ self class
		julianDayNumber: jdn
		seconds: seconds + diff totalSeconds
		nanoseconds: nanos + diff nanoSeconds
		offset: anOffset! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 6/4/2014 09:41'!
julianDayNumber: aJulianDayNumber

	^ self
		julianDayNumber: aJulianDayNumber
		seconds: 0
		nanoseconds: 0
		offset: self localOffset! !

!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'jmv 6/4/2014 09:40'!
fromSeconds: seconds 
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"

	| integerSeconds nanos |
	integerSeconds _ seconds truncated.
	nanos _ integerSeconds = seconds
		ifTrue: [ 0 ]
		ifFalse: [ (seconds - integerSeconds * Time nanosInSecond) asInteger ].
	^ self
		julianDayNumber: Time squeakEpoch
		seconds: integerSeconds
		nanoseconds: nanos
		offset: self localOffset! !


!Time methodsFor: 'ansi protocol' stamp: 'jmv 6/3/2014 19:02'!
= aTime

	"Any object is equal to itself"
	self == aTime ifTrue: [ ^ true ].

	self class == aTime class ifFalse: [ ^false ].
	^ seconds = aTime privateSeconds and: [ nanos = aTime privateNanos ]! !

!Time methodsFor: 'ansi protocol' stamp: 'jmv 6/3/2014 19:07'!
hash

	^ seconds bitXor: nanos! !


!Time class methodsFor: 'squeak protocol' stamp: 'jmv 6/4/2014 06:49'!
seconds: seconds
	"Answer a Time from midnight."

	^ self afterMidnight: (Duration seconds: seconds)! !

!Time class methodsFor: 'squeak protocol' stamp: 'jmv 6/4/2014 06:49'!
seconds: seconds nanoSeconds: nanoCount
	"Answer a Time from midnight."

	^ self afterMidnight: (Duration seconds: seconds nanoSeconds: nanoCount)! !


!TimeTest reorganize!
('tests' testCreation testEqual)
!


!DateAndTimeTest reorganize!
('tests' testCreation testOperations)
!

!methodRemoval: Time #ticks!
Time removeSelector: #ticks!
!methodRemoval: Time #ticks:!
Time removeSelector: #ticks:!
!methodRemoval: Duration #ticks!
Duration removeSelector: #ticks!
!methodRemoval: DateAndTime #hasEqualTicks:!
DateAndTime removeSelector: #hasEqualTicks:!
!methodRemoval: DateAndTime #normalize:ticks:base:!
DateAndTime removeSelector: #normalize:ticks:base:!
!methodRemoval: DateAndTime #ticks!
DateAndTime removeSelector: #ticks!
!methodRemoval: DateAndTime #ticks:offset:!
DateAndTime removeSelector: #ticks:offset:!
