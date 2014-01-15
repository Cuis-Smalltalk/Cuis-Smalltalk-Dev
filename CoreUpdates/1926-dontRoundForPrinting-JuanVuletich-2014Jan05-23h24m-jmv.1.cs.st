'From Cuis 4.2 of 25 July 2013 [latest update: #1925] on 5 January 2014 at 11:29:22.011995 pm'!

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/5/2014 21:39'!
tallySendsTo: receiver inBlock: aBlock showTree: treeOption
	"
	MessageTally tallySends: [3.14159 printString]
	"
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. If receiver is not nil, then only sends
	to that receiver are tallied.
	Results are presented as leaves, sorted by frequency,
	preceded, optionally, by the whole tree."
	| prev tallies startTime totalTime |
	startTime _ Time localMillisecondClock.
	tallies _ MessageTally new class: aBlock receiver class method: aBlock method.
	tallies reportOtherProcesses: true.	"Do NOT filter nodes with nil process"
	prev _ aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :current |
			current == prev ifFalse: [ "call or return"
				prev sender ifNotNil: [ "call only"
					(receiver == nil or: [ current receiver == receiver ])
						ifTrue: [ tallies tally: current by: 1 ]].
				prev _ current]].

	totalTime _ Time localMillisecondClock - startTime / 1000.0.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [ :s |
				s
					nextPutAll: 'This simulation took ';
					nextPutAll: totalTime printString;
					nextPutAll: ' seconds.';
					newLine.
				treeOption
					ifTrue: [ tallies fullPrintExactOn: s ]
					ifFalse: [ tallies leavesPrintExactOn: s ]]))
			label: 'Spy Results'
			wrap: false ]! !


!Integer methodsFor: 'printing' stamp: 'jmv 1/5/2014 21:35'!
asBytesDescription
	"Answer a terse, easily-readable representation of this Integer reprsenting a number of bytes.  Useful for file-browsers.
	123 asBytesDescription
	(12*1024) asBytesDescription
	1024 asBytesDescription
	(1024*1024) asBytesDescription
	(1024*1024*1024) asBytesDescription
	(1024*1024*1024*1024) asBytesDescription
	(30 factorial) asBytesDescription
	"
	| suffixes |
	suffixes _ { 'K'"ilobytes". 'M'"egabytes". 'G'"igabytes". 'T'"erabytes". 'P'"etabytes". 'E'"xabytes". 'Z'"ettabytes". 'Y'"ottabytes"}.
	suffixes size to: 1 by: -1 do: [ :index |  | units |
		units _ 1024 raisedTo: index.
		self >= units ifTrue: [
			^(String streamContents: [ :strm |
				(self / units) printOn: strm fractionDigits: 2.
				strm nextPutAll: (suffixes at: index)
				])]].
	^ self asString! !


!Time class methodsFor: 'benchmarks' stamp: 'jmv 1/5/2014 22:58'!
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
	^String streamContents: [ :strm |
		delayTime - time * 1000.0 / nLoops printOn: strm fractionDigits: 3.
		strm nextPutAll: ' microseconds per call to Time>>millisecondClockValue' ]! !

!Time class methodsFor: 'benchmarks' stamp: 'jmv 1/5/2014 23:01'!
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
	^String streamContents: [ :strm |
		(actualTime - baseTime) * 1000 asFloat / (nLoops * 10) printOn: strm fractionDigits: 3.
		strm nextPutAll: ' microseconds overhead per primitive call' ]! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 1/5/2014 22:44'!
printSpaceAnalysis: threshold on: aStream
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace _ totalInstCount _ totalInstSpace _ n _ 0.
	results _ OrderedCollection new: Smalltalk classNames size.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :bar |
			Smalltalk allClassesDo: [ :cl |
				codeSpace _ cl spaceUsed.
				bar value: (n _ n+1).
				Smalltalk garbageCollectMost.
				instCount _ cl instanceCount.
				instSpace _ (cl indexIfCompact > 0 ifTrue: [4] ifFalse: [8]) * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!!"
				cl isVariable
					ifTrue: [
						eltSize _ cl isBytes ifTrue: [1] ifFalse: [4].
						cl allInstancesDo: [ :x |
							instSpace _ instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace _ instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace _ totalCodeSpace + codeSpace.
				totalInstCount _ totalInstCount + instCount.
				totalInstSpace _ totalInstSpace + instSpace]].
	totalPercent _ 0.0.

	aStream timeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results _ (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent _ s spaceForInstances*100.0/totalInstSpace.
		totalPercent _ totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 1/5/2014 22:52'!
vmStatisticsReportString
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime upTime2 fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |
	params := Smalltalk getVMParameters.
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memoryEnd			:= params at: 3.
	fullGCs				:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs				:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.
	upTime := Time millisecondClockValue.

	str := WriteStream on: (String new: 1000).
	str	nextPutAll: 'uptime			';
		print: (upTime / 1000 / 60 // 60); nextPut: $h;
		print: (upTime / 1000 / 60 \\ 60) asInteger; nextPut: $m;
		print: (upTime / 1000 \\ 60) asInteger; nextPut: $s; newLine.

	str	nextPutAll: 'memory			';
		nextPutAll: memoryEnd asStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd asStringWithCommas; nextPutAll: ' bytes ('.
	oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd - oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd asStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommas; nextPutAll: ' bytes ('.
	memoryEnd - youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.

	str	nextPutAll: 'GCs				';
		nextPutAll: (fullGCs + incrGCs) asStringWithCommas.
	fullGCs + incrGCs > 0 ifTrue: [
		str
			nextPutAll: ' ('; 
			print: ((upTime / (fullGCs + incrGCs)) rounded); 
			nextPutAll: 'ms between GCs)'
	].
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime / upTime * 100) rounded);
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse: [
		str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime / upTime * 100) rounded);
		nextPutAll: '% uptime), avg '; print: ((incrGCTime / incrGCs) rounded); nextPutAll: 'ms'; newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount asStringWithCommas.
	tenureCount = 0 ifFalse: [
		str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.

LastStats ifNil: [LastStats := Array new: 6]
ifNotNil: [
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).

	str	nextPutAll: self textMarkerForShortReport ;
		nextPutAll: (fullGCs2 + incrGCs2) asStringWithCommas.
	fullGCs2 + incrGCs2 > 0 ifTrue: [
		str
			nextPutAll: ' ('; 
			print: ((upTime2 / (fullGCs2 + incrGCs2)) rounded); 
			nextPutAll: 'ms between GCs)'.
	].
	str newLine.
	str	nextPutAll: '	uptime		'; print: ((upTime2 / 1000.0) rounded); nextPutAll: 's'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs2; nextPutAll: ' totalling '; nextPutAll: fullGCTime2 asStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime2 / upTime2 * 100) rounded);
		nextPutAll: '% uptime)'.
	fullGCs2 = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime2 / fullGCs2) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs2; nextPutAll: ' totalling '; nextPutAll: incrGCTime2 asStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime2 / upTime2 * 100) rounded);
		nextPutAll: '% uptime), avg '.
	incrGCs2 > 0 ifTrue: [
		 str print: ((incrGCTime2 / incrGCs2) rounded); nextPutAll: 'ms'
	].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount2 asStringWithCommas.
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
].
	LastStats at: 1 put: upTime.
	LastStats at: 2 put: fullGCs.
	LastStats at: 3 put: fullGCTime.
	LastStats at: 4 put: incrGCs.
	LastStats at: 5 put: incrGCTime.
	LastStats at: 6 put: tenureCount.

	^ str contents
! !

