'From Cuis 4.2 of 25 July 2013 [latest update: #2420] on 12 July 2015 at 3:11:16.287989 pm'!
!classDefinition: #MessageTally category: #'Tools-Profiling'!
Magnitude subclass: #MessageTally
	instanceVariableNames: 'class method process tally receivers senders time gcStats reportOtherProcesses '
	classVariableNames: 'DefaultPollPeriod Timer '
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!MessageTally commentStamp: '<historical>' prior: 0!
My instances observe and report the amount of time spent in methods.

MessageTally provides two different strategies available for profiling:

* tallySends: and friends use the interpreter simulator to run the block, recording every method call. It gives you accurate counts of how many times methods get called, and by exactly which route. If you're debugging, or trying to figure out if a given method is getting called too many times, this is your tool.

* spyOn: and friends use a high-priority Process to interrupt the block or process being spied on at periodic intervals. The interrupted call stack is then examined for caller information. This was moved to AndreasSystemProfiler.!


!AndreasSystemProfiler class methodsFor: 'instance creation' stamp: 'jmv 7/12/2015 14:24'!
new
	self canWork ifFalse: [
		self error: 'AndreasSystemProfiler primitives not present in this VM' ].
	^super new! !


!MessageTally methodsFor: 'printing' stamp: 'jmv 7/12/2015 15:03'!
treePrintExactOn: aStream tabs: tabs thisTab: myTab

	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	tabs size > 0 
		ifTrue: [
			self printOn: aStream ].
	sons _ receivers.
	sons isEmpty 
		ifFalse: [
			tabs addLast: myTab.
			sons _ sons asArray sort.
			(1 to: sons size) do: [ :i | 
					sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
					(sons at: i)
						treePrintExactOn: aStream
						tabs: tabs
						thisTab: sonTab ].
			tabs removeLast]! !


!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 7/12/2015 14:24'!
runBlock: aBlock
"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
"
	| result linesStream talliesStream textStream |

	tally := AndreasSystemProfiler new.
	tally observedProcess: Processor activeProcess.
	result := tally spyOn: aBlock.

	textStream _ DummyStream on: nil.
	linesStream _ WriteStream on: Array new.
	talliesStream _ WriteStream on: Array new.
	
	tally reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream.
	self initializeMessageList: linesStream contents talliesList: talliesStream contents.

	self changed: #messageList.
	self changed: #messageListIndex.
	self changed: #decorateButtons.
	^result! !


!Debugger methodsFor: 'private' stamp: 'jmv 7/12/2015 14:34'!
process: aProcess context: aContext

	interruptedProcess _ aProcess.
	contextStackTop _ aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex _ 1.
	externalInterrupt _ false.
	selectingPC _ true! !


!MessageTally methodsFor: 'collecting leaves' stamp: 'jmv 7/12/2015 15:02'!
into: leafDict fromSender: senderTally

	| leafNode |
	leafNode _ leafDict at: method
		ifAbsent: [
			leafDict at: method
				put: (MessageTally new class: class method: method)].
	leafNode bump: tally fromSender: senderTally! !

!MessageTally methodsFor: 'collecting leaves' stamp: 'jmv 7/12/2015 15:01'!
leavesInto: leafDict fromSender: senderTally

	| rcvrs |
	rcvrs _ self sonsOver: 0.
	rcvrs size = 0
		ifTrue: [ self into: leafDict fromSender: senderTally ]
		ifFalse: [
			rcvrs do: [ :node |
				node isPrimitives
					ifTrue: [ node leavesInto: leafDict fromSender: senderTally ]
					ifFalse: [ node leavesInto: leafDict fromSender: self ]]]! !

!MessageTally methodsFor: 'comparing' stamp: 'jmv 7/12/2015 14:56'!
= aMessageTally

	self species == aMessageTally species ifFalse: [^ false].
	^ aMessageTally method == method! !

!MessageTally methodsFor: 'comparing' stamp: 'jmv 7/12/2015 14:56'!
sonsOver: threshold

	| hereTally sons |
	(receivers isNil or: [ receivers size = 0 ]) ifTrue: [ ^#() ].
	hereTally _ tally.
	sons _ receivers select: [ :son | "subtract subNode tallies for primitive hits here"
		hereTally _ hereTally - son tally.
		son tally > threshold ].
	hereTally > threshold
		ifTrue: [
			| last |
			last _ MessageTally new class: class method: method.
			^sons copyWith: (last primitives: hereTally)].
	^sons! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 7/12/2015 14:59'!
fullPrintExactOn: aStream

	aStream nextPutAll: '**Tree**'; newLine.
	self
		treePrintExactOn: aStream
		tabs: OrderedCollection new
		thisTab: ''.
	aStream nextPut: Character newPage; newLine.
	aStream nextPutAll: '**Leaves**'; newLine.
	self leavesPrintExactOn: aStream! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 7/12/2015 15:02'!
leavesPrintExactOn: aStream

	| dict |
	dict _ IdentityDictionary new: 100.
	self leavesInto: dict fromSender: nil.
	dict asArray sort
		do: [ :node |
			node printOn: aStream.
			node printSenderCountsOn: aStream ]! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 7/12/2015 15:03'!
printOn: aStream

	| myTally aSelector aClass |
	myTally := tally.
	receivers
		ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
	aStream
		print: myTally;
		space.
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: aSelector;
				newLine ]! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 7/12/2015 15:03'!
printSenderCountsOn: aStream

	| mergedSenders |
	mergedSenders _ IdentityDictionary new.
	senders do: [ :node | | mergedNode |
		mergedNode _ mergedSenders at: node method ifAbsent: nil .
		mergedNode
			ifNil: [ mergedSenders at: node method put: node ]
			ifNotNil: [ mergedNode bump: node tally ]].
	mergedSenders asArray sort do: [ :node | 
		10 to: node tally printString size by: -1 do: [ :i | aStream space ].
		node printOn: aStream ]! !

!MessageTally methodsFor: 'tallying' stamp: 'jmv 7/12/2015 15:03'!
tallyPath: context by: count

	| aMethod path |
	aMethod _ context method.
	
	"Find the correct child (if there)"
	receivers do: [ :oldTally | 
		oldTally method == aMethod ifTrue: [path _ oldTally]].
	
	"Add new child if needed"
	path ifNil: [
		path _ MessageTally new class: context receiver class method: aMethod.
		receivers _ receivers copyWith: path].
	
	^ path bumpBy: count! !

!MessageTally methodsFor: 'private' stamp: 'jmv 7/12/2015 15:00'!
copyWithTally: hitCount

	^ (MessageTally new class: class method: method)
		bump: hitCount! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 7/12/2015 14:28'!
spyOn: aBlock
	"
	Spy on aBlock, in the current process. Do not include statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyOn: [10000 timesRepeat: [1.23 printString]]
	"
	"
	MessageTally spyOn:[10000 timesRepeat: [3.14159 printString]]
	"

	^AndreasSystemProfiler spyOn: aBlock! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 7/12/2015 14:54'!
tallySendsTo: receiver inBlock: aBlock
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
				tallies fullPrintExactOn: s ]))
			label: 'Spy Results'
			wrap: false ]! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 7/12/2015 14:36'!
startMessageTally
	"Tally on all the processes in the system, and not only the UI"
	
	(self confirm: 'MessageTally all the processes in
the system, until the mouse pointer
goes to the top of the screen') ifTrue: [
		AndreasSystemProfiler spyAllOn: [
			[Sensor peekMousePt y > 0] whileTrue: [myWorld doOneCycle]]]! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 7/12/2015 14:33'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| stream profiler |
	
	"VM statistics (Memory use and GC, mainly)"
	stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'MemoryStats.txt'.
	[ stream nextPutAll: Utilities vmStatisticsReportString ] 
		ensure: [ stream close ].
	
	"Process list"
	stream _ FileStream forceNewFileNamed: 'ProcessList.txt'.
	[
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]
	] ensure: [ stream close ].

"Fork all these, so they run in sequence, as the system is back running"
[
	
	"Process taking most CPU"
	stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'ThePig.txt'.
	ProcessBrowser dumpPigStackOn: stream andClose: true.
	
	"Tally of all processes"
	AndreasSystemProfiler canWork ifTrue: [
		stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'FullTally.txt'.
		[
			profiler _ AndreasSystemProfiler new.
			profiler spyOn: [ (Delay forMilliseconds: 1000) wait ].
			profiler
				reportTextOn: stream
				linesOn: (DummyStream on: nil)
				talliesOn: (DummyStream on: nil)
		] ensure: [ stream close ]].

	"Memory Analysis"
	stream _ FileDirectory smalltalkImageDirectory forceNewFileNamed: 'MemoryAnalysis.txt'.
	[ SpaceTally new printSpaceAnalysis: 1 on: stream ]
		ensure: [ stream close ]

] forkNamed: 'CPU usage analysis'! !

!methodRemoval: MessageTally class #defaultPollPeriod!
MessageTally class removeSelector: #defaultPollPeriod!
!methodRemoval: MessageTally class #releaseClassCachedState!
MessageTally class removeSelector: #releaseClassCachedState!
!methodRemoval: MessageTally class #spyAllOn:!
MessageTally class removeSelector: #spyAllOn:!
!methodRemoval: MessageTally class #spyOn:reportOtherProcesses:!
MessageTally class removeSelector: #spyOn:reportOtherProcesses:!
!methodRemoval: MessageTally class #terminateTimerProcess!
MessageTally class removeSelector: #terminateTimerProcess!

!MessageTally class reorganize!
('spying' spyOn: tallySends: tallySendsTo:inBlock:)
!

!methodRemoval: MessageTally #fullPrintOn:threshold:!
MessageTally removeSelector: #fullPrintOn:threshold:!
!methodRemoval: MessageTally #leavesPrintOn:threshold:!
MessageTally removeSelector: #leavesPrintOn:threshold:!
!methodRemoval: MessageTally #printOn:total:totalTime:tallyExact:!
MessageTally removeSelector: #printOn:total:totalTime:tallyExact:!
!methodRemoval: MessageTally #process!
MessageTally removeSelector: #process!
!methodRemoval: MessageTally #process:!
MessageTally removeSelector: #process:!
!methodRemoval: MessageTally #report:!
MessageTally removeSelector: #report:!
!methodRemoval: MessageTally #report:cutoff:!
MessageTally removeSelector: #report:cutoff:!
!methodRemoval: MessageTally #reportGCStatsOn:!
MessageTally removeSelector: #reportGCStatsOn:!
!methodRemoval: MessageTally #reportOtherProcesses:!
MessageTally removeSelector: #reportOtherProcesses:!
!methodRemoval: MessageTally #rootPrintOn:total:totalTime:threshold:!
MessageTally removeSelector: #rootPrintOn:total:totalTime:threshold:!
!methodRemoval: MessageTally #spyAllEvery:on:!
MessageTally removeSelector: #spyAllEvery:on:!
!methodRemoval: MessageTally #spyEvery:on:!
MessageTally removeSelector: #spyEvery:on:!
!methodRemoval: MessageTally #tally:in:by:!
MessageTally removeSelector: #tally:in:by:!
!methodRemoval: MessageTally #tallyPath:in:by:!
MessageTally removeSelector: #tallyPath:in:by:!
!methodRemoval: MessageTally #time!
MessageTally removeSelector: #time!
!methodRemoval: MessageTally #treePrintOn:tabs:thisTab:total:totalTime:tallyExact:orThreshold:!
MessageTally removeSelector: #treePrintOn:tabs:thisTab:total:totalTime:tallyExact:orThreshold:!
!classDefinition: #MessageTally category: #'Tools-Profiling'!
Magnitude subclass: #MessageTally
	instanceVariableNames: 'class method tally receivers senders'
	classVariableNames: 'DefaultPollPeriod Timer'
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!MessageTally reorganize!
('collecting leaves' bump: bump:fromSender: into:fromSender: leavesInto:fromSender:)
('comparing' < = > hash isPrimitives sonsOver:)
('printing' fullPrintExactOn: leavesPrintExactOn: printOn: printSenderCountsOn: treePrintExactOn:tabs:thisTab:)
('reporting' tally)
('tallying' bumpBy: tally:by: tallyPath:by:)
('private' class:method: copyWithTally: method primitives:)
!

