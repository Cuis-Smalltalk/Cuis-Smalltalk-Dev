'From Cuis 4.1 of 12 December 2012 [latest update: #1675] on 17 April 2013 at 12:40:44 am'!

!Time class methodsFor: 'primitives' stamp: 'jmv 4/16/2013 23:31'!
primHighResClock	
	"Primitive. Answer the value of the high resolution clock if this computer has one.
	Usually, this should be the highest resolution value available, for example on Intel
	it will be the value of the time stamp counter register."
"
	Time primHighResClock.
	On Cog on OS-X, it seems to give sub nano second precision!!

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

!Time class methodsFor: 'primitives' stamp: 'jmv 4/16/2013 23:38'!
primUtcMicrosecondClock
	"Answer the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in UTC time.
	Answer is (at least usually) a  LargePositiveInteger"
	"
	Time primUtcMicrosecondClock
	"
	<primitive: 240>
	self primitiveFailed! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/16/2013 23:14'!
primUtcWithOffset
	"Answer a two element array.

	First element is the number of microseconds since the UTC Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in UTC time.
	It is (at least usually) a  LargePositiveInteger.
	
	Second element is the current seconds offset from GMT in the local time zone.
	
	Warning 1: When this primitive was discussed in VM-Dev, first element was said to be in Unix epoch (1/1/1970), but it seems it is in Smalltalk epoch.
	
	Warning 2: I (jmv) can not make sense of the second element. I get 62581760, that is 	almost two years.
	
	Because of this, Cuis is not currently using this primitive.
	"
	"
	Time primUtcWithOffset
	"
	<primitive: 'primitiveUtcWithOffset'>
	^nil! !

!Time class methodsFor: 'private' stamp: 'jmv 4/16/2013 23:23'!
currentUtcOffset
	"Find out the current offset from UTC by calling both a primitive that answers UTC time and one that answers local time.
	As there is a slight difference in time between both primitive calls, round the answer to whole minutes."
	"
	Time currentUtcOffset
	"
	| localSeconds utcMicroseconds |
	utcMicroseconds _ self primUtcMicrosecondClock.
	localSeconds _ self primSecondsClock.
	^(localSeconds - (utcMicroseconds / 1000000.0) / 60.0) rounded minutes! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 00:17'!
millisecondsInDay
	^86400000! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 00:32'!
nanosInSecond
	^ 1000000000! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 00:19'!
secondsInDay
	^86400! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 00:34'!
secondsInHour
	^3600! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 00:33'!
secondsInMinute
	^60! !

!Time class methodsFor: 'constants' stamp: 'jmv 4/17/2013 00:36'!
squeakEpoch
	^ 2415386. 		"Julian day number of 1 Jan 1901"! !


!Month class methodsFor: 'inquiries' stamp: 'jmv 4/17/2013 00:30'!
daysInMonth
	^#(31 28 31 30 31 30 31 31 30 31 30 31)! !

!Month class methodsFor: 'inquiries' stamp: 'jmv 4/17/2013 00:29'!
monthNames
	^#(January February March April May June July August September October November December)! !


!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 4/16/2013 23:32'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop method leaf |
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	tallyStart := tallyStop := Time primHighResClock.
	[ true ]
		whileTrue: [ 
			tallyStart := tallyStop.
			tallyStart := Time primHighResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyStop := Time primHighResClock.
			tallyTicks := tallyStop - tallyStart.
			totalTicks := totalTicks + tallyTicks.
			process := Smalltalk profileSample.
			method := Smalltalk profilePrimitive.
			totalTally := totalTally + 1.
			process
				ifNotNil: [ 
					leaf := tallyRoot
						tally: (process suspendedContext ifNil: [ thisContext ])
						in: process
						by: tallyTicks.
					method ifNotNil: [ leaf tallyMethod: method by: tallyTicks ] ] ]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 4/16/2013 23:32'!
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	| t0 |
	semaphore := Semaphore new.
	"Run a 100 msec busy loop to figure out the ticks per msec"
	t0 := Time millisecondClockValue + 2.
	[Time millisecondClockValue >= t0] whileFalse.
	ticksPerMSec := Time primHighResClock.
	[Time millisecondClockValue >= (t0 + 100)] whileFalse.
	ticksPerMSec := (Time primHighResClock - ticksPerMSec) 
			// (Time millisecondClockValue - t0).
	"Try to get 10 samples per msec"
	ticks := ticksPerMSec // 10.
	startTime := Time millisecondClockValue.
	vmStats := Smalltalk getVMParameters.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: profilerProcess.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume! !


!AndreasSystemProfiler class methodsFor: 'testing' stamp: 'jmv 4/16/2013 23:32'!
canWork
	"
	AndreasSystemProfiler canWork
	"
	Time primHighResClock = 0 ifTrue: [ ^false ].
	[
		Smalltalk profileSemaphore: nil.
		Smalltalk profileStart: 0 ] on: Error do: [ ^false ].
	^true! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:35'!
- operand
	"operand is a DateAndTime or a Duration"

	(operand isKindOf: DateAndTime)
		ifTrue: [
			| lticks rticks |
			lticks _ self asLocal ticks.
			rticks _ operand asLocal ticks.
			^Duration
 				seconds: (Time secondsInDay *(lticks first - rticks first)) + 
							(lticks second - rticks second)
 				nanoSeconds: (lticks third - rticks third) ].
	
	^self + operand negated! !

!DateAndTime methodsFor: 'private' stamp: 'jmv 4/17/2013 00:35'!
ticks: ticks offset: utcOffset
	"ticks is {julianDayNumber. secondCount. nanoSeconds}"

	self normalize: 3 ticks: ticks base: Time nanosInSecond.
	self normalize: 2 ticks: ticks base: Time secondsInDay.

	jdn	_ ticks at: 1.
	seconds	_ ticks at: 2.
	nanos := ticks at: 3.
	offset := utcOffset! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:37'!
epoch
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self julianDayNumber: Time squeakEpoch! !

!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:38'!
milliSecondsSinceMidnight
	| msm msClock |

	"This is usually only during system startup..."
	self waitForOffsets.

	msClock _ Time millisecondClockValue.
	msClock < LastMilliSeconds ifTrue: [ "rolled over"
		MilliSecondOffset _ MilliSecondOffset + (SmallInteger maxVal // 2) + 1 ].
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

!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:37'!
fromSeconds: seconds 
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"

	| integerSeconds nanos |
	integerSeconds _ seconds truncated.
	nanos _ integerSeconds = seconds
		ifTrue: [ 0 ]
		ifFalse: [ (seconds - integerSeconds * Time nanosInSecond) asInteger ].
	^ self basicNew
		ticks: (Array
				with: Time squeakEpoch
				with: integerSeconds
				with: nanos)
		offset: self localOffset! !

!DateAndTime class methodsFor: 'initialize-release' stamp: 'jmv 4/16/2013 22:08'!
initialize
	"
	self initialize
	"
	LastTickSemaphore := Semaphore forMutualExclusion.
	LastMilliSeconds := 0.
	LastTick := 0.
	Smalltalk addToStartUpList: self.
	self startUp: true! !

!DateAndTime class methodsFor: 'initialize-release' stamp: 'jmv 4/17/2013 00:38'!
initializeOffsets
	| durationSinceEpoch secondsSinceMidnight nowSecs |
	LastTick _ 0.
	nowSecs _ Time secondsWhenClockTicks.
	LastMilliSeconds _ Time millisecondClockValue.
	durationSinceEpoch _ Duration
		days: Time squeakEpoch
		hours: 0
		minutes: 0
		seconds: nowSecs.
	DaysSinceEpoch _ durationSinceEpoch days.
	secondsSinceMidnight _ (durationSinceEpoch -
		(Duration
			days: DaysSinceEpoch
			hours: 0
			minutes: 0
			seconds: 0)) totalSeconds.
	MilliSecondOffset _ secondsSinceMidnight * 1000 - LastMilliSeconds! !


!Debugger methodsFor: 'notifier menu' stamp: 'jmv 4/16/2013 22:16'!
storeLog
	| logFileName |
	logFileName _ Preferences debugLogTimestamp
		ifTrue: ['CuisDebug-' , Time primSecondsClock printString , '.log']
		ifFalse: ['CuisDebug.log'].
	Smalltalk logError: labelString printString inContext: contextStackTop to: logFileName
! !


!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 4/16/2013 22:16'!
markRecentlyUsed
	timestamp := Time primSecondsClock! !


!Duration methodsFor: 'public protocol' stamp: 'jmv 4/17/2013 00:31'!
totalMilliSeconds

	^ self totalNanoSeconds // 1000000! !

!Duration methodsFor: 'public protocol' stamp: 'jmv 4/17/2013 00:32'!
totalNanoSeconds

	^ (seconds * Time nanosInSecond) + nanos! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:35'!
days
	"Answer the number of days the receiver represents."

	^ seconds quo: Time secondsInDay! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:35'!
hours
	"Answer the number of hours the receiver represents."


	^ (seconds rem: Time secondsInDay) quo: Time secondsInHour! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:34'!
minutes
	"Answer the number of minutes the receiver represents."

	^ (seconds rem: Time secondsInHour) quo: Time secondsInMinute! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:34'!
seconds
	"Answer the number of seconds the receiver represents."

	^seconds rem: Time secondsInMinute! !

!Duration methodsFor: 'private' stamp: 'jmv 4/17/2013 00:32'!
seconds: secondCount nanoSeconds: nanoCount 
	"Private - only used by Duration class"

	seconds := secondCount.
	nanos := nanoCount rounded.
	"normalize if signs do not match"
	[ nanos < 0 and: [ seconds > 0 ] ]
		whileTrue: [ seconds := seconds - 1.
			nanos := nanos + Time nanosInSecond ].
	[ seconds < 0 and: [ nanos > 0 ] ]
		whileTrue: [ seconds := seconds + 1.
			nanos := nanos - Time nanosInSecond ]

! !

!Duration methodsFor: 'private' stamp: 'jmv 4/17/2013 00:35'!
ticks
	"Answer an array {days. seconds. nanoSeconds}. Used by DateAndTime and Time."

	| days |
	days _ self days.
	^ Array 
		with: days
		with: seconds - (days * Time secondsInDay)
		with: nanos! !


!Duration class methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:35'!
days: days seconds: seconds

	^ self basicNew seconds: days * Time secondsInDay + seconds nanoSeconds: 0! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:35'!
days: aNumber

	^ self seconds: aNumber * Time secondsInDay nanoSeconds: 0! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:35'!
days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: nanos	

 	^ self seconds: ((days * Time secondsInDay) 
						+ (hours * Time secondsInHour)
							+ (minutes * Time secondsInMinute) 
								+ seconds)
		nanoSeconds: nanos! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:35'!
hours: aNumber

	^ self seconds: aNumber * Time secondsInHour nanoSeconds: 0! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:31'!
milliSeconds: milliCount

	^ self nanoSeconds: milliCount * 1000000! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:34'!
minutes: aNumber

	^ self seconds: aNumber * Time secondsInMinute nanoSeconds: 0! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:32'!
nanoSeconds: nanos
	"This method is slow. If you have nanos less than 10^6 you should use #seconds:nanoSeconds: instead."

	| quo |
	quo _ nanos quo: Time nanosInSecond.
	^ self basicNew
		seconds: quo
		nanoSeconds: nanos - (quo * Time nanosInSecond)! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:32'!
seconds: seconds nanoSeconds: nanos

	^ self basicNew
		seconds: seconds truncated
		nanoSeconds: seconds fractionPart * Time nanosInSecond + nanos! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 4/17/2013 00:19'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preferences standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ ((Date fromSeconds: entry modificationTime) printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			(Time fromSeconds: entry modificationTime \\ Time secondsInDay)
				print24: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize asStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas + sizePad - sizeDigits.
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/17/2013 00:27'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	ClassicTheme beCurrent.
	PasteUpMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	CursorWithAlpha class removeSelector: #buildBiggerNormal.
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	"????
	Smalltalk organization removeCategoriesMatching: 'Signal Processing*'.
	SystemOrganization removeSystemCategory: 'LinearAlgebra'.
	Smalltalk organization removeCategoriesMatching: 'Sound-*'
	"

	Beeper setDefault: nil.
	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(ZipConstants GZipConstants ZipFileConstants SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSorter zapAllChangeSets.
	Smalltalk garbageCollect.


	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!Time methodsFor: 'printing' stamp: 'jmv 4/17/2013 00:33'!
print24: hr24 showSeconds: showSeconds on: aStream 
	"Format is 'hh:mm:ss' or 'h:mm:ss am'  or, if showSeconds is false, 'hh:mm' or 'h:mm am'"

	| h m s |
	h := self hour. m := self minute. s := self second.
	hr24
		ifTrue: 
			[ h < 10 ifTrue: [ aStream nextPutAll: '0' ].
			h printOn: aStream ]
		ifFalse:
			[ h > 12
				ifTrue: [h - 12 printOn: aStream]
				ifFalse: 
					[h < 1
						ifTrue: [ 12 printOn: aStream ]
						ifFalse: [ h printOn: aStream ]]].

	aStream nextPutAll: (m < 10 ifTrue: [':0'] ifFalse: [':']).
	m printOn: aStream.

	showSeconds ifTrue:
		[ aStream nextPutAll: (s < 10 ifTrue: [':0'] ifFalse: [':']).
		self nanoSecond = 0
			ifTrue: [s asInteger printOn: aStream]
			ifFalse: [(s + (self nanoSecond / Time nanosInSecond) asFloat) printOn: aStream]].

	hr24 ifFalse:
		[ aStream nextPutAll: (h < 12 ifTrue: [' am'] ifFalse: [' pm']) ].
! !

!Time methodsFor: 'private' stamp: 'jmv 4/17/2013 00:33'!
seconds: secondCount nanoSeconds: nanoCount 
	"Private - only used by Time class."

	self assert: nanoCount < Time nanosInSecond.
	seconds _ secondCount.
	nanos _ nanoCount! !

!Time methodsFor: 'private' stamp: 'jmv 4/17/2013 00:33'!
ticks: anArray
	"ticks is an Array: { days. seconds. nanoSeconds }"

	seconds _ anArray at: 2.
	nanos _ anArray at: 3.
	self assert: nanos < Time nanosInSecond! !


!Time class methodsFor: 'benchmarks' stamp: 'jmv 4/16/2013 22:02'!
benchmarkMillisecondClock
	"
	Time benchmarkMillisecondClock
	"
	"Benchmark the time spent in a call to Time>>millisecondClockValue.
	On the VM level this tests the efficiency of calls to ioMSecs()."
	"PII/400 Windows 98: 0.725 microseconds per call"
	| temp1 temp2 temp3 delayTime nLoops time |
	delayTime := 5000. "Time to run benchmark is approx. 2*delayTime"

	"Don't run the benchmark if we have an active delay since
	we will measure the additional penalty in the primitive dispatch
	mechanism (see #benchmarkPrimitiveResponseDelay)."
	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	"Flush the cache for this benchmark so we will have
	a clear cache hit for each send to #millisecondClockValue below"
	Object flushCache.
	temp1 := 0.
	temp2 := self. "e.g., temp1 == Time"
	temp3 := self millisecondClockValue + delayTime.

	"Now check how often we can run the following loop in the given time"
	[temp2 millisecondClockValue < temp3]
		whileTrue:[temp1 := temp1 + 1].

	nLoops := temp1. "Remember the loops we have run during delayTime"

	"Setup the second loop"
	temp1 := 0.
	temp3 := nLoops.

	"Now measure how much time we spend without sending #millisecondClockValue"
	time := Time millisecondClockValue.
	[temp1 < temp3]
		whileTrue:[temp1 := temp1 + 1].
	time := Time millisecondClockValue - time.

	"And compute the number of microseconds spent per call to #millisecondClockValue"
	^((delayTime - time * 1000.0 / nLoops) truncateTo: 0.001) printString,
		' microseconds per call to Time>>millisecondClockValue'! !

!Time class methodsFor: 'benchmarks' stamp: 'jmv 4/16/2013 22:03'!
benchmarkPrimitiveResponseDelay
	"
	Time benchmarkPrimitiveResponseDelay
	"
	"Benchmark the overhead for primitive dispatches with an active Delay.
	On the VM level, this tests the efficiency of ioLowResMSecs."

	"PII/400 Windows98: 0.128 microseconds per prim"

	"ar 9/6/1999: This value is *extremely* important for stuff like sockets etc.
	I had a bad surprise when Michael pointed this particular problem out:
	Using the hardcoded clock() call for ioLowResMSecs on Win32 resulted in an overhead
	of 157.4 microseconds per primitive call - meaning you can't get no more than
	approx. 6000 primitives per second on my 400Mhz PII system with an active delay!!
	BTW, it finally explains why Squeak seemed soooo slow when running PWS or 
	other socket stuff. The new version (not using clock() but some Windows function) 
	looks a lot better (see above; approx. 8,000,000 prims per sec with an active delay)."

	| nLoops bb index baseTime actualTime delayTime |
	delayTime := 5000. "Time to run this test is approx. 3*delayTime"

	Delay anyActive ifTrue:[
		^self notify:'Some delay is currently active.
Running this benchmark will not give any useful result.'].

	bb := Array new: 1. "The object we send the prim message to"

	"Compute the # of loops we'll run in a decent amount of time"
	[(Delay forMilliseconds: delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	nLoops := 0.
	[Delay anyActive] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		nLoops := nLoops + 1.
	].

	"Flush the cache and make sure #basicSize is in there"
	Object flushCache.
	bb basicSize.

	"Now run the loop without any active delay
	for getting an idea about its actual speed."
	baseTime := self millisecondClockValue.
	index := nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index := index - 1.
	].
	baseTime := self millisecondClockValue - baseTime.

	"Setup the active delay but try to never make it active"
	[(Delay forMilliseconds: delayTime + delayTime) wait] 
		forkAt: Processor userInterruptPriority.

	"And run the loop"
	actualTime := self millisecondClockValue.
	index := nLoops.
	[index > 0] whileTrue:[
		bb basicSize; basicSize; basicSize; basicSize; basicSize; 
			basicSize; basicSize; basicSize; basicSize; basicSize.
		index := index - 1.
	].
	actualTime := self millisecondClockValue - actualTime.

	"And get us some result"
	^((actualTime - baseTime) * 1000 asFloat / (nLoops * 10) truncateTo: 0.001) printString,
		' microseconds overhead per primitive call'! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/16/2013 22:16'!
condenseBunches: aCollectionOfSeconds
	"Identify the major intervals in a bunch of numbers.  Each number is a seconds since 1901 that represents a date and time.  We want the last event in a bunch.  Return array of seconds for:
	Every event in the last half hour.
	Every bunch separated by 30 min in the last 24 hours.
	Every bunch separated by two hours before that."
	"Time condenseBunches: 
	(#(20 400 401  20000 20200 20300   40000 45000  200000 201000 202000) collect: [:tt | 
		self primSecondsClock - tt]) 
"
	| secArray pause now out prev bunchEnd ago |
	secArray _ aCollectionOfSeconds asArray sort.
	pause _ 1.
	now _ self primSecondsClock.
	out _ OrderedCollection new.
	prev _ 0.
	bunchEnd _ nil.
	secArray reverseDo: [ :secs |
		"descending"
		ago _ now - secs.
		ago > (60 * 30) ifTrue: [ pause _ 1800 "60*30" ].
		ago > (60 * 60 * 24) ifTrue: [ pause _ 7200 "60*120" ].
		ago - prev >= pause ifTrue: [
			out add: bunchEnd.
			bunchEnd _ secs ].
		prev _ ago ].
	out add: bunchEnd.
	out removeFirst.
	^ out.! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 00:21'!
humanWordsForSecondsAgo: secs
	| date today |
	"Return natural language for this date and time in the past."

	secs <= 1 ifTrue: [^ 'a second ago'].
	secs < 45 ifTrue: [^ secs printString, ' seconds ago'].
	secs < 90 ifTrue: [^ 'a minute ago'].
	secs < "45*60" 2700 ifTrue: [^ (secs//self secondsInMinute) printString, ' minutes ago'].
	secs < "90*60" 5400 ifTrue: [^ 'an hour ago'].
	secs < "18*60*60" 64800 ifTrue: [^ (secs//self secondsInHour) printString, ' hours ago'].
	date _ Date fromSeconds: self primSecondsClock - secs.		"now work with dates"
	today _ Date today.
	date > (today - 2 days) ifTrue: [^ 'yesterday'].
	date > (today - 8 days) ifTrue: [^ 'last ', date dayOfWeekName].
	date > (today - 13 days) ifTrue: [^ 'a week ago'].
	date > (today - 28 days) ifTrue: [
		^ ((today - date) days //7) printString, ' weeks ago'].
	date > (today - 45 days) ifTrue: [^ 'a month ago'].
	date > (today - 300 days) ifTrue: [^ 'last ', date monthName].
	^ date monthName, ', ', date yearNumber printString

"Example
#(0.5 30 62 130 4000 10000 60000 90000 345600 864000 1728000 3456000 17280000 34560000 345600000) 
		collect: [:ss | Time humanWordsForSecondsAgo: ss].
"! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/16/2013 22:28'!
millisecondClockValue
	"Answer the number of milliseconds since the millisecond clock was last reset or rolled over.
	Answer 0 if the primitive fails."

	^ self primMillisecondClock! !

!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/16/2013 22:17'!
namesForTimes: arrayOfSeconds
	| simpleEnglish prev final prevPair myPair |
	"Return English descriptions of the times in the array.  They are each seconds since 1901.  If two names are the same, append the date and time to distinguish them."

	simpleEnglish _ arrayOfSeconds collect: [:secsAgo |
		self humanWordsForSecondsAgo: self primSecondsClock - secsAgo].
	prev _ ''.
	final _ simpleEnglish copy.
	simpleEnglish withIndexDo: [:eng :ind | 
		eng = prev ifFalse: [eng]
			ifTrue: ["both say 'a month ago'"
				prevPair _ self dateAndTimeFromSeconds: 
						(arrayOfSeconds at: ind-1).
				myPair _ self dateAndTimeFromSeconds: 
						(arrayOfSeconds at: ind).
				(final at: ind-1) = prev ifTrue: ["only has 'a month ago'"
					final at: ind-1 put: 
							(final at: ind-1), ', ', prevPair first mmddyyyy].
				final at: ind put: 
							(final at: ind), ', ', myPair first mmddyyyy.
				prevPair first = myPair first 
					ifTrue: [
						(final at: ind-1) last == $m ifFalse: ["date but no time"
							final at: ind-1 put: 
								(final at: ind-1), ', ', prevPair second printMinutes].
						final at: ind put: 
							(final at: ind), ', ', myPair second printMinutes]].
		prev _ eng].
	^ final! !

!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:22'!
dateAndTimeFromSeconds: secondCount

	^ Array
		with: (Date fromSeconds: secondCount)
		with: (Time fromSeconds: secondCount \\ self secondsInDay)
! !

!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 4/16/2013 22:16'!
dateAndTimeNow
	"Answer a two-element Array of (Date today, Time now)."

	^ self dateAndTimeFromSeconds: self primSecondsClock! !

!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:33'!
fromSeconds: secondCount 
	"Answer an instance of me that is secondCount number of seconds since midnight."

	| integerSeconds nanos |
	integerSeconds := secondCount truncated.
	integerSeconds = secondCount
		ifTrue: [nanos := 0]
		ifFalse: [nanos := (secondCount - integerSeconds * Time nanosInSecond) asInteger].
	^ self seconds: integerSeconds nanoSeconds: nanos
! !

!Time class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:35'!
hour: hour minute: minute second: second nanoSecond: nanoCount
	"Answer a Time"

	^ self 
		seconds: (hour * Time secondsInHour) + (minute * Time secondsInMinute) + second 
		nanoSeconds: nanoCount! !

!Time class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:36'!
noon

	^ self seconds: Time secondsInDay / 2! !

!Time class methodsFor: 'ansi protocol' stamp: 'jmv 4/16/2013 22:14'!
now
	"Answer a Time representing the time right now - this is a 24 hour clock."

	| ms |
	ms _ DateAndTime milliSecondsSinceMidnight.
	^ self seconds:  (ms // 1000) nanoSeconds: (ms \\ 1000) * 1000000! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/16/2013 22:43'!
primMillisecondClock
	"Primitive. Answer the number of milliseconds since the millisecond clock
	was last reset or rolled over. No sync to any system clock.
	Answer zero if the primitive fails.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 135>
	^ 0! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/17/2013 00:24'!
primMillisecondClockMask
	"Answer the mask value used for millisecond clock rollover in the
	virtual machine, or nil if the VM cannot support the request."

	<primitive: 'primitiveMillisecondClockMask'>
	^ nil! !

!Time class methodsFor: 'primitives' stamp: 'jmv 4/16/2013 23:01'!
primSecondsClock
	"Answer the number of seconds since the Smalltalk epoch, 
	i.e. 00:00 on the morning of January 1, 1901, in the local host time zone.
	In other words, the number of seconds since it was that time in this time zone.
	Answer is a 32-bit unsigned number, and (at least usually) a LargePositiveInteger.
	Note: This is in local time, i.e. the time the system shows to the user.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 137>
	self primitiveFailed! !

!Time class methodsFor: 'private' stamp: 'jmv 4/16/2013 22:11'!
initializeMillisecondClockMask
	"Initialize cached value from the VM, or set to nil if VM cannot support the request"

	MillisecondClockMask _ self primMillisecondClockMask! !

!Time class methodsFor: 'private' stamp: 'jmv 4/16/2013 22:11'!
secondsWhenClockTicks
	"waits for the moment when a new second begins"
	| lastSecond delay |

	delay _ Delay forMilliseconds: 1.
	lastSecond _ self primSecondsClock.
	[ lastSecond = self primSecondsClock ] whileTrue: [ delay wait ].
 
	^ lastSecond + 1! !


!TimeTest methodsFor: 'Tests' stamp: 'jmv 4/16/2013 22:17'!
testSqueakInquiries
	| timewords totalseconds condensed corrected |
	self assert: 
		(Time namesForTimes: #(2 10000023 10000026))
			= #('January, 1901' 'April, 1901, 4/26/1901, 5:47 pm' 'April, 1901, 4/26/1901, 5:47 pm').

	timewords := #(0.5 30 62 130 4000 10000 60000 86401) 
		collect: [ :ss | Time humanWordsForSecondsAgo: ss ].
	self assert: 
		timewords = #('a second ago' '30 seconds ago' 'a minute ago' '2 minutes ago' 
			'an hour ago' '2 hours ago' '16 hours ago' 'yesterday').

	totalseconds :=  Time primSecondsClock.
	condensed := Time condenseBunches: 
		(#(20 400 401  20000 20200 20300 40000 45000  200000 201000 202000) 
			collect: [:tt | totalseconds - tt]).
	corrected := condensed collect: [ :e | totalseconds - e ].
	self
		assert: (corrected includesAllOf: #(20 400 401 20000 40000 45000 200000)).
! !


!Timespan class methodsFor: 'squeak protocol' stamp: 'jmv 4/16/2013 22:26'!
current
	"
	Date current
	Week current
	Month current
	"

	^ self including: DateAndTime now! !


!Date class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:36'!
fromDays: dayCount 
	"Days since 1 January 1901"

	^ self julianDayNumber: dayCount + Time squeakEpoch! !


!Month class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:30'!
daysInMonth: indexOrName forYear: yearInteger 

	| index |
	index _ indexOrName isInteger 
				ifTrue: [indexOrName]
				ifFalse: [self indexOfMonth: indexOrName].
	^ (self daysInMonth at: index)
			+ ((index = 2
					and: [Year isLeapYear: yearInteger])
						ifTrue: [1] ifFalse: [0])! !

!Month class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:29'!
indexOfMonth: aMonthName


	1 to: 12 do: [ :i |  (aMonthName, '*' match: (self monthNames at: i)) ifTrue: [^i] ].
	self error: aMonthName , ' is not a recognized month name'! !

!Month class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:29'!
nameOfMonth: anIndex

	^ self monthNames at: anIndex! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 4/17/2013 00:23'!
monthDayTime24StringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the compact form
             ddMMMhhmm		where dd is a two-digit day-of-month, MMM is the alpha month abbreviation and hhmm is the time on a 24-hr clock.

          Utilities monthDayTime24StringFrom: Time primSecondsClock
"

	aDate _ Date fromSeconds: aSecondCount.
	aTime _ Time fromSeconds: aSecondCount \\ Time secondsInDay.

	^ (aDate dayOfMonth asTwoCharacterString), 
		(aDate monthName copyFrom: 1 to: 3), 
		(aTime hhmm24)! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 4/17/2013 00:23'!
monthDayTimeStringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the form:
		ddMMMhhmmPP	  where:
							dd is a two-digit day-of-month,
							MMM is the alpha month abbreviation,
							hhmm is the time,
							PP is either am or pm

          Utilities monthDayTimeStringFrom: Time primSecondsClock
"

	aDate _ Date fromSeconds: aSecondCount.
	aTime _ Time fromSeconds: aSecondCount \\ Time secondsInDay.

	^ (aDate dayOfMonth asTwoCharacterString), 
		(aDate monthName copyFrom: 1 to: 3), 
		((aTime hour \\ 12) asTwoCharacterString), 
		(aTime minute asTwoCharacterString),
		(aTime hour > 12 ifTrue: ['pm'] ifFalse: ['am'])! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:28'!
including: aDateAndTime
	"Week will start from the 
		Week startDay
	"

	| midnight delta adjusted |
	midnight _ aDateAndTime midnight.
	delta _ ((midnight dayOfWeek + 7 - (self dayNames indexOf: self startDay)) rem: 7) abs.
	adjusted _ midnight - delta days.

	^ self basicNew
 		start: adjusted;
		duration: (Duration weeks: 1);
		yourself! !

!Week class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:28'!
indexOfDay: aSymbol

	^ self dayNames indexOf: aSymbol! !

!Week class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:28'!
startDay
	^ StartDay ifNil: [ StartDay _ self dayNames first ]! !

!Week class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:28'!
startDay: aSymbol

	(self dayNames includes: aSymbol)
		ifTrue: [ StartDay _ aSymbol ]
		ifFalse: [ self error: aSymbol, ' is not a recognised day name' ]! !

!Week class methodsFor: 'smalltalk-80' stamp: 'jmv 4/17/2013 00:28'!
nameOfDay: anIndex

	^ self dayNames at: anIndex! !

!Week class methodsFor: 'inquiries' stamp: 'jmv 4/17/2013 00:29'!
dayNames

	^ #(Sunday Monday Tuesday Wednesday Thursday Friday Saturday)! !


!ZipStringMember methodsFor: 'initialization' stamp: 'jmv 4/16/2013 22:17'!
contents: aString
	contents _ aString.
	compressedSize _ uncompressedSize _ aString size.
	"set the file date to now"
	self setLastModFileDateTimeFrom: Time primSecondsClock! !


!Week class reorganize!
('squeak protocol' including: indexOfDay: startDay startDay:)
('smalltalk-80' nameOfDay:)
('inquiries' classDefinesDuration dayNames mustEncompassWholeDates)
!

!methodRemoval: TimeTest #timeClass!
TimeTest removeSelector: #timeClass!
!methodRemoval: Time class #SqueakEpoch!
Time class removeSelector: #SqueakEpoch!
!methodRemoval: Time class #current!
Time class removeSelector: #current!
!methodRemoval: Time class #milliSecondsSinceMidnight!
Time class removeSelector: #milliSecondsSinceMidnight!
!methodRemoval: Time class #totalSeconds!
Time class removeSelector: #totalSeconds!

!Time class reorganize!
('benchmarks' benchmarkMillisecondClock benchmarkPrimitiveResponseDelay)
('general inquiries' condenseBunches: humanWordsForSecondsAgo: millisecondClockMask millisecondClockValue milliseconds:since: millisecondsSince: millisecondsToRun: namesForTimes:)
('smalltalk-80' dateAndTimeFromSeconds: dateAndTimeNow fromSeconds: new readFrom:)
('squeak protocol' fromString: hour:minute:second: hour:minute:second:nanoSecond: midnight noon seconds: seconds:nanoSeconds:)
('ansi protocol' now)
('primitives' primHighResClock primMicrosecondClock primMillisecondClock primMillisecondClockMask primSecondsClock primUtcMicrosecondClock primUtcWithOffset)
('private' currentUtcOffset initializeMillisecondClockMask secondsWhenClockTicks)
('constants' millisecondsInDay nanosInSecond secondsInDay secondsInHour secondsInMinute squeakEpoch)
!

!methodRemoval: DateAndTime class #clock!
DateAndTime class removeSelector: #clock!
!methodRemoval: DateAndTime class #current!
DateAndTime class removeSelector: #current!
!methodRemoval: DateAndTime class #millisecondClockValue!
DateAndTime class removeSelector: #millisecondClockValue!
!methodRemoval: DateAndTime class #totalSeconds!
DateAndTime class removeSelector: #totalSeconds!
DateAndTime initialize!
!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos'
	classVariableNames: 'DaysSinceEpoch LastMilliSeconds LastTick LastTickSemaphore LocalTimeZone MilliSecondOffset OffsetsAreValid'
	poolDictionaries: 'ChronologyConstants'
	category: 'Kernel-Chronology'!