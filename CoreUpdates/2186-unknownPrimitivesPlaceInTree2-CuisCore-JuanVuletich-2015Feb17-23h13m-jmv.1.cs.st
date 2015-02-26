'From Cuis 4.2 of 25 July 2013 [latest update: #2184] on 17 February 2015 at 11:14:13.872604 pm'!
!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/17/2015 20:38'!
tallyFakeBy: count
	"Called explicitly and needs to decrement receiver's tally count"
	"???"
	
	"Not really nice. Currently disabled. See comment at sender."
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node isFake ifTrue:[^node addTallies: count].
		node := node nextLink.
	].
	node := QFakeTally new.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count! !


!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 2/17/2015 20:57'!
runBlock: aBlock pollingEvery: pollPeriod 
	| stream list result |
	block := MessageSend 
				receiver: self
				selector: #runBlock:pollingEvery:
				arguments: { 
						aBlock.
						pollPeriod}.	"so we can re-run it"
	tally := MessageTally new.
	tally reportOtherProcesses: false.
	result := tally spyEvery: pollPeriod on: aBlock.
	stream := ReadWriteStream 
				with: (String streamContents: [ :s | 
					tally report: s]).
	stream reset.
	list := OrderedCollection new.
	[stream atEnd] whileFalse: [list add: stream nextLine].
	self initializeMessageList: list.
	self changed: #messageList.
	self changed: #messageListIndex.
	self changed: #decorateButtons.
	^result! !

!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 2/17/2015 20:57'!
runProcess: aProcess forMilliseconds: msecDuration pollingEvery: pollPeriod 
	| stream list result |
	block := MessageSend 
				receiver: self
				selector: #runProcess:forMilliseconds:pollingEvery: 
				arguments: { 
						aProcess.
						msecDuration.
						pollPeriod}.	"so we can re-run it"
	tally := MessageTally new.
	tally reportOtherProcesses: false.
	result := tally 
				spyEvery: pollPeriod
				onProcess: aProcess
				forMilliseconds: msecDuration.
	stream := ReadWriteStream 
				with: (String streamContents: [ :s | 
							tally report: s]).
	stream reset.
	list := OrderedCollection new.
	[stream atEnd] whileFalse: [list add: stream nextLine].
	self initializeMessageList: list.
	self changed: #messageList.
	self changed: #messageListIndex.
	self changed: #decorateButtons.
	^result! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 2/17/2015 21:09'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime := totalTime.
	oldSpaceEnd		:= vmStats at: 1.
	youngSpaceEnd		:= vmStats at: 2.
	memoryEnd			:= vmStats at: 3.
	fullGCs				:= vmStats at: 7.
	fullGCTime			:= vmStats at: 8.
	incrGCs				:= vmStats at: 9.
	incrGCTime			:= vmStats at: 10.
	tenureCount			:= vmStats at: 11.
	rootOverflows		:= vmStats at: 22.

	str newLine.
	str	nextPutAll: '**Memory**'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (fullGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: (fullGCTime / fullGCs) rounded; nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms (';
		print: (incrGCTime / upTime * 100) rounded;
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: (incrGCTime / incrGCs) rounded; nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows printStringWithCommas; nextPutAll:' overflows'.
	str newLine.
! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/17/2015 21:03'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop methodForPrimitiveWhileTakingSample parentNode |
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
			methodForPrimitiveWhileTakingSample := Smalltalk profilePrimitive.
			totalTally := totalTally + 1.
			process
				ifNotNil: [
					methodForPrimitiveWhileTakingSample
						ifNil: [
							tallyRoot
								tally: (process suspendedContext ifNil: [ thisContext ])
								inProcess: process
								by: tallyTicks.
							]
						ifNotNil: [
							"The intention of this code is record which primitive was running when the VM took the sample."
							"In Eliot Miranda's words: 
								AndreasSystemProfiler is more accurate because it uses VM support to tell it which primtiive was running when it took a sample. 
								MessageTally simply ascribes a primitive's cost to the method at the next suspendsion point, which, in some contexts, 
								can yield wildly misleading results."
							"The problem is that knowing just the primitive and the process doesn't give us the complete call stack.
							So, this is, in a sense, approximate."
							"
								AndreasSystemProfiler spyOn: [
							 	       [ #((1 2 3)) do: [ :each |
 								               each findLast: [ :ea |
								                        ea squared = ea ] ] ] bench ].
							The older code, for this example
								AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
							gave:
								  |  2.9% (7 ms) (Number>>#raisedToInteger:)
								  |    2.2% (5 ms) (Float>>#timesTwoPower: )
							but #raisedToInteger: does NOT send #timesTwoPower:"
							parentNode _ tallyRoot
								tally: (process suspendedContext ifNil: [ thisContext ]) sender
								inProcess: process
								by: tallyTicks.
							(parentNode tallyFakeBy: tallyTicks)
								tallyMethod: methodForPrimitiveWhileTakingSample by: tallyTicks
							]]]! !


!Process methodsFor: 'printing' stamp: 'jmv 2/17/2015 20:49'!
browserPrintStringFull: aBoolean

	^String streamContents: [ :stream |
		aBoolean ifTrue: [
			stream
				nextPutAll: self statusString;
				space ].
		priority printOn: stream.
		stream nextPutAll: ' ('.
		self hash printOn: stream.
		stream
			nextPutAll: ') ';
			nextPutAll: self name.
		self animatedUI ifNotNil: [ :ui |
			stream nextPutAll: ' - '.
			ui printOn: stream ].
		aBoolean ifTrue: [
			stream
				nextPutAll: ': ';
				nextPutAll: suspendedContext asString ]]! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 2/17/2015 21:00'!
printOn: aStream total: total totalTime: totalTime

	| aSelector aClass percentage |
	percentage := tally asFloat / total * 100.0.
	percentage printOn: aStream fractionDigits: 1.
	aStream
		nextPutAll: '% (';
		nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
		nextPutAll: ' ms) '.

	aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			blockNesting > 0 ifTrue: [
				aStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
	aStream
		nextPutAll: class name;
		nextPutAll: (aClass = class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
		nextPutAll: aSelector;
		newLine! !

!QSystemTally methodsFor: 'initialization' stamp: 'jmv 2/17/2015 20:40'!
class: aClass method: aCompiledMethod nesting: blockNestingCount
	class := aClass.
	method := aCompiledMethod.
	blockNesting := blockNestingCount! !


!MessageTally methodsFor: 'printing' stamp: 'jmv 2/17/2015 20:56'!
printOn: aStream 
	| aSelector aClass |
	(class isNil or: [method isNil]) ifTrue: [^super printOn: aStream].
	aSelector := class selectorAtMethod: method setClass: [:c | aClass := c].
	aStream
		nextPutAll: aClass name;
		nextPutAll: ' >> ';
		nextPutAll: aSelector! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 2/17/2015 20:56'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact 

	isExact 
		ifTrue: [
			| myTally |
			myTally := tally.
			receivers
				ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
			aStream
				print: myTally;
				space]
		ifFalse: [
			| percentage |
			percentage := tally asFloat / total * 100.0.
			percentage printOn: aStream fractionDigits: 1.
			aStream
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
				nextPutAll: 'ms) '].
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			| aSelector aClass |
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: aSelector;
				newLine ]! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 2/17/2015 20:51'!
treePrintOn: aStream tabs: tabs thisTab: myTab total: total totalTime: totalTime tallyExact: isExact orThreshold: threshold 
	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	tabs size > 0 
		ifTrue: [
			self 
				printOn: aStream
				total: total
				totalTime: totalTime
				tallyExact: isExact].
	sons _ isExact ifTrue: [ receivers ] ifFalse: [ self sonsOver: threshold ].
	sons isEmpty 
		ifFalse: [
			tabs addLast: myTab.
			sons _ sons asArray sort.
			(1 to: sons size) do: [ :i | 
					sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
					(sons at: i) 
						treePrintOn: aStream
						tabs: tabs
						thisTab: sonTab
						total: total
						totalTime: totalTime
						tallyExact: isExact
						orThreshold: threshold ].
			tabs removeLast]! !

!MessageTally methodsFor: 'tallying' stamp: 'jmv 2/17/2015 20:57'!
tallyPath: context in: aProcess by: count
	| aMethod path |
	aMethod _ context method.
	
	"Find the correct child (if there)"
	receivers do: [ :oldTally | 
		(oldTally method == aMethod and: [oldTally process == aProcess])
			ifTrue: [path _ oldTally]].
		
	"Add new child if needed"
	path ifNil:[
		path _ MessageTally new class: context receiver class method: aMethod;
			process: aProcess;
			reportOtherProcesses: reportOtherProcesses.
		receivers _ receivers copyWith: path].

	^ path bumpBy: count! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 2/17/2015 21:06'!
spyAllOn: aBlock
	"Spy on all the processes in the system
	
	[1000 timesRepeat: [3.14159 printString. Processor yield]] forkNamed: 'p1'.
	[1000 timesRepeat: [30 factorial. Processor yield]] forkNamed: 'p2'.
	[1000 timesRepeat: [30 factorial. Processor yield]] forkNamed: 'p3'.
	MessageTally spyAllOn: [ (Delay forMilliseconds: 100) wait]

	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyAllOn: [10000 timesRepeat: [1.23 printString]]
	"
	| node result |
	"
	AndreasSystemProfiler canWork ifTrue: [
		^AndreasSystemProfiler spyAllOn: aBlock ].
	"
	node _ self new.
	node reportOtherProcesses: true.	"Irrelevant in this case. All processes will be reported on their own."
	result _ node spyAllEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Classic MessageTally Spy Results (AndreasSystemProfiler primitives not present in this VM)'
			wrap: false ].
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 2/17/2015 21:06'!
spyOn: aBlock reportOtherProcesses: aBoolean
	"
	Spy on aBlock, in the current process. Can include or not statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyOn: [10000 timesRepeat: [1.23 printString]] reportOtherProcesses: true
	"
	| node result |
	"
	AndreasSystemProfiler canWork ifTrue: [
		^AndreasSystemProfiler spyOn: aBlock includeAllProcesses: aBoolean ].
	"
	node _ self new.
	node reportOtherProcesses: aBoolean.
	result _ node spyEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Classic MessageTally Spy Results (AndreasSystemProfiler primitives not present in this VM)'
			wrap: false ].
	^ result! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 2/17/2015 21:09'!
vmStatisticsReportString
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |
	params := Smalltalk getVMParameters.
	oldSpaceEnd		:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memoryEnd			:= params at: 3.
	fullGCs				:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs				:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.

	str := WriteStream on: (String new: 1000).

	str	nextPutAll: 'memory			';
		nextPutAll: memoryEnd printStringWithCommas; nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old			';
		nextPutAll: oldSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd - oldSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd printStringWithCommas; nextPutAll: ' bytes ('.
	youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) printStringWithCommas; nextPutAll: ' bytes ('.
	memoryEnd - youngSpaceEnd / memoryEnd * 100 printOn: str fractionDigits: 1.
	str
		nextPutAll: '%)';
		newLine.

	str	nextPutAll: 'GCs				';
		nextPutAll: (fullGCs + incrGCs) printStringWithCommas.
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime printStringWithCommas; nextPutAll: 'ms'.
	fullGCs = 0 ifFalse: [
		str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime printStringWithCommas; nextPutAll: 'ms';
		nextPutAll: ', avg '; print: ((incrGCTime / incrGCs) rounded); nextPutAll: 'ms'; newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.

LastStats ifNil: [LastStats := Array new: 6]
ifNotNil: [
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).

	str	nextPutAll: self textMarkerForShortReport ;
		nextPutAll: (fullGCs2 + incrGCs2) printStringWithCommas.
	str newLine.
	str	nextPutAll: '	full			';
		print: fullGCs2; nextPutAll: ' totalling '; nextPutAll: fullGCTime2 printStringWithCommas; nextPutAll: 'ms'.
	fullGCs2 = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime2 / fullGCs2) rounded); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr			';
		print: incrGCs2; nextPutAll: ' totalling '; nextPutAll: incrGCTime2 printStringWithCommas; nextPutAll: 'ms';
		nextPutAll: ', avg '.
	incrGCs2 > 0 ifTrue: [
		 str print: ((incrGCTime2 / incrGCs2) rounded); nextPutAll: 'ms'
	].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount2 printStringWithCommas.
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
].
	LastStats at: 2 put: fullGCs.
	LastStats at: 3 put: fullGCTime.
	LastStats at: 4 put: incrGCs.
	LastStats at: 5 put: incrGCTime.
	LastStats at: 6 put: tenureCount.

	^ str contents
! !

!methodRemoval: MessageTally class #defaultMaxClassNameSize!
MessageTally class removeSelector: #defaultMaxClassNameSize!
!methodRemoval: MessageTally class #defaultMaxClassPlusSelectorSize!
MessageTally class removeSelector: #defaultMaxClassPlusSelectorSize!
!methodRemoval: MessageTally class #defaultMaxTabs!
MessageTally class removeSelector: #defaultMaxTabs!
!methodRemoval: MessageTally #initialize!
MessageTally removeSelector: #initialize!
!methodRemoval: MessageTally #maxClassNameSize!
MessageTally removeSelector: #maxClassNameSize!
!methodRemoval: MessageTally #maxClassNameSize:!
MessageTally removeSelector: #maxClassNameSize:!
!methodRemoval: MessageTally #maxClassPlusSelectorSize!
MessageTally removeSelector: #maxClassPlusSelectorSize!
!methodRemoval: MessageTally #maxClassPlusSelectorSize:!
MessageTally removeSelector: #maxClassPlusSelectorSize:!
!methodRemoval: MessageTally #maxTabs!
MessageTally removeSelector: #maxTabs!
!methodRemoval: MessageTally #maxTabs:!
MessageTally removeSelector: #maxTabs:!
!classDefinition: #MessageTally category: #'Tools-Profiling'!
Magnitude subclass: #MessageTally
	instanceVariableNames: 'class method process tally receivers senders time gcStats reportOtherProcesses'
	classVariableNames: 'DefaultPollPeriod Timer'
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!methodRemoval: QSystemTally #addTallies:!
QSystemTally removeSelector: #addTallies:!
!methodRemoval: QSystemTally #addToLeafReportDict:!
QSystemTally removeSelector: #addToLeafReportDict:!
!methodRemoval: QSystemTally #asArray!
QSystemTally removeSelector: #asArray!
!methodRemoval: QSystemTally #fullPrintOn:threshold:time:reportOnly:!
QSystemTally removeSelector: #fullPrintOn:threshold:time:reportOnly:!
!methodRemoval: QSystemTally #leavesIntoLeafReportDict:!
QSystemTally removeSelector: #leavesIntoLeafReportDict:!
!methodRemoval: QSystemTally #leavesPrintOn:threshold:time:!
QSystemTally removeSelector: #leavesPrintOn:threshold:time:!
!methodRemoval: QSystemTally #maxClassNameSize!
QSystemTally removeSelector: #maxClassNameSize!
!methodRemoval: QSystemTally #maxClassPlusSelectorSize!
QSystemTally removeSelector: #maxClassPlusSelectorSize!
!methodRemoval: QSystemTally #maxTabs!
QSystemTally removeSelector: #maxTabs!
!methodRemoval: QSystemTally #process!
QSystemTally removeSelector: #process!
!methodRemoval: QSystemTally #process:!
QSystemTally removeSelector: #process:!
!methodRemoval: QSystemTally #rootPrintOn:total:totalTime:threshold:reportOnly:!
QSystemTally removeSelector: #rootPrintOn:total:totalTime:threshold:reportOnly:!
!methodRemoval: QSystemTally #sonsOver:!
QSystemTally removeSelector: #sonsOver:!
!methodRemoval: QSystemTally #sortCollectionFrom:!
QSystemTally removeSelector: #sortCollectionFrom:!
!methodRemoval: QSystemTally #tally!
QSystemTally removeSelector: #tally!
!methodRemoval: QSystemTally #tallyMethod:by:!
QSystemTally removeSelector: #tallyMethod:by:!
!methodRemoval: QSystemTally #treePrintOn:tabs:thisTab:total:totalTime:threshold:!
QSystemTally removeSelector: #treePrintOn:tabs:thisTab:total:totalTime:threshold:!
!classDefinition: #QSystemTally category: #'Tools-Profiling'!
QAbstractTally subclass: #QSystemTally
	instanceVariableNames: 'class method blockNesting'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!QSystemTally reorganize!
('report' printOn: printOn:total:totalTime:)
('tallying' blockNestingCountOf: tally:inProcess:by: tallyFakeBy: tallyLeafChild:inProcess:by:)
('accessing' blockNestingCount method)
('initialization' class:method:nesting:)
!

