'From Cuis 4.1 of 12 December 2012 [latest update: #1578] on 26 January 2013 at 11:51:24 pm'!
!classDefinition: #AndreasSystemProfiler category: #AndreasProfiler!
Object subclass: #AndreasSystemProfiler
	instanceVariableNames: 'semaphore ticks profilerProcess tallyRoot vmStats totalTally totalTime startTime ticksPerMSec totalTicks observedProcess'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AndreasProfiler'!

!AndreasSystemProfiler commentStamp: '<historical>' prior: 0!
AndreasSystemProfiler uses sub-msec VM supported PC sampling.

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Released by Ron, Julie and David

Some technical details from Eliot Miranda ( http://forum.world.st/AndreasSystemProfiler-Released-MIT-tp4664958p4665182.html ):

Both AndreasSystemProfiler and MessageTally are periodic sampling profilers.  The essential difference between AndreasSystemProfiler and MessageTally is in how the current method is sampled.

MessageTally is driven from a high-priority process in a loop waiting on a delay.  When the delay fires the lower-priority process being profiled is interrupted, its stack is walked to determine the methods along the call chain, and that data is recorded.  But since the sampling occurs when the high-priority process preempts the lower-priority process, a sample is only taken at a preemption point.  In particular, primitives are *not* profiled because they are not suspension points.  A process can only be suspended on method activation (a non-primitive method activation, or primitive failure) or on backward branch.  The cost of primitives is charged to a caller and is inferred by subtracting the cost of children of the caller from the caller itself (subtracting the number of samples in children of the caller form the number of samples in the caller itself).  

Another problem is that using the clock that underlies Delay, which is typically the clock used by processes being profiled, causes sampling errors due to the sampling and sampled processes cohering.  Delays are limited in resolution (at best 1 millisecond) so if the profiled process waits on a delay it'll fire immediately after the profiling process (because the profiling process is at higher priority) and so the sampling process may only ever see the sampled process in a wait state.

If MessageTally is used to profile multiple processes then a third problem is that if a primitive causes a process switch then its cost will end up being charged to the process switched-to, not switched from.  This is again because sampling can only occur after a primitive has completed (successfully or not).

AndreasSystemProfiler is driven from a high-priority process in a loop waiting on a Semaphore known to the VM.  The profiling process uses a primitive to schedule a sample some number of ticks of the VM's high-performance clock in the future.  When the time is reached the VM samples the current method and the current process, *before any process preemption takes place*, and independently of the standard clock, and signals the semaphore.  The profiling process then collects the method,process pair via primitives.  So AndreasSystemProfiler provides much more accurate results.

That said there are still limitations with primitives and Cog.  Currently Cog only samples "interpreter" primitives.  Those primitives it implements in machine code (integer and float arithmetic, closure evaluation, at:, identityHash) are not sampled and won't show up; they will be charged to the calling method.  This is fixable, since Cog actually compiles the sampling direct into interpreter primitive invocation when profiling is in effect and not at other times, but sampling could be a significant cost in these simple and performance-critical primitives.!

!classDefinition: #QSystemTally category: #AndreasProfiler!
Link subclass: #QSystemTally
	instanceVariableNames: 'class method process receivers tally senders'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'AndreasProfiler'!

!QSystemTally commentStamp: '<historical>' prior: 0!
Tally for assembling system profiles. It's a subclass of Link so we can chain tallies together in the receivers list.!


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 11/12/2012 17:27'!
doReport
	"Report the results of this profiler run"
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: self report)
			label: 'Spy Results'
			wrap: false ]! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'ar 6/11/2007 22:55'!
report
	"Answer a report, with cutoff percentage of each element of the tree"
	^String streamContents:[:s| self report: s].! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'ar 2/26/2008 18:37'!
report: strm 
	"Print a report, with cutoff percentage of each element of the tree 
	(leaves, roots, tree)=2, on the stream, strm."

	self report: strm cutoff: 1! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 11/12/2012 15:41'!
report: strm cutoff: threshold
	tallyRoot tally = 0
		ifTrue: [ strm nextPutAll: ' - no tallies obtained' ]
		ifFalse: [
			strm
				nextPutAll: 'Reporting - ' , totalTally asStringWithCommas;
				nextPutAll: ' tallies, ' , totalTime asStringWithCommas , ' msec.';
				newLine;
				newLine.
			tallyRoot
				fullPrintOn: strm
				threshold: threshold
				time: totalTime
				reportOnly: observedProcess ].
	totalTime isZero ifFalse: [
		self reportGCStatsOn: strm.
		self reportProcessStatsOn: strm ].! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 11/11/2012 22:28'!
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime := totalTime.
	oldSpaceEnd			:= vmStats at: 1.
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
		nextPutAll: oldSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	young		';
		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	used		';
		nextPutAll: youngSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.

	str newLine.
	str	nextPutAll: '**GCs**'; newLine.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((fullGCTime / upTime * 100) roundTo: 1.0);
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) roundTo: 1.0); nextPutAll: 'ms'].
	str	newLine.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';
		print: ((incrGCTime / upTime * 100) roundTo: 1.0);
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: ((incrGCTime / incrGCs) roundTo: 1.0); nextPutAll: 'ms'].
	str newLine.
	str	nextPutAll: '	tenures		';
		nextPutAll: tenureCount asStringWithCommas.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	newLine.
	str	nextPutAll: '	root table	';
		nextPutAll: rootOverflows asStringWithCommas; nextPutAll:' overflows'.
	str newLine.
! !

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 11/11/2012 22:28'!
reportProcessStatsOn: str
	| totalSwitches pageOverflows pageDivorces actualSwitches |
	vmStats size >= 61 ifFalse:[^self]. "don't try this on the closure VM"
	totalSwitches := vmStats at: 56.
	actualSwitches := totalSwitches - (2*totalTally). "exclude to/from profiler"
	pageOverflows := vmStats at: 60.
	pageDivorces := vmStats at: 61.

	str newLine.
	str nextPutAll: '**Processes**'; newLine.
	str tab; nextPutAll: 'Total process switches: '; print: totalSwitches; newLine.
	str tab; nextPutAll: 'Without Profiler: '; print: actualSwitches; newLine.
	str tab; nextPutAll: 'Stack page overflows: '; print: pageOverflows; newLine.
	str tab; nextPutAll: 'Stack page divorces: '; print: pageDivorces; newLine.
! !

!AndreasSystemProfiler methodsFor: 'accessing' stamp: 'jmv 11/12/2012 11:53'!
observedProcess: aProcess
	observedProcess _ aProcess! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 11/12/2012 16:29'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop method leaf |
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	tallyStart := tallyStop := Smalltalk highResClock.
	[ true ]
		whileTrue: [ 
			tallyStart := tallyStop.
			tallyStart := Smalltalk highResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyStop := Smalltalk highResClock.
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

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 11/12/2012 17:38'!
spyOn: aBlock
	"Profile system activity during execution of aBlock. 
	The argument is the desired samples per *milli* second.
	Mostly for polymorphism with MessageTally."
	tallyRoot := QSystemTally new class: aBlock receiver class method: aBlock method.
	self startProfiling.
	^aBlock ensure: [self stopProfiling]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 1/25/2013 01:28'!
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	| t0 |
	semaphore := Semaphore new.
	"Run a 100 msec busy loop to figure out the ticks per msec"
	t0 := Time millisecondClockValue + 2.
	[Time millisecondClockValue >= t0] whileFalse.
	ticksPerMSec := Smalltalk highResClock.
	[Time millisecondClockValue >= (t0 + 100)] whileFalse.
	ticksPerMSec := (Smalltalk highResClock - ticksPerMSec) 
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

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 12/26/2012 13:30'!
stopProfiling
	"Stop the profiler process"
	Smalltalk profileSemaphore: nil.
	Smalltalk profileStart: 0. "<- profile stops now"
	totalTime := Time millisecondClockValue - startTime.
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :value | 
		value isNumber ifTrue: [
			vmStats at: idx put: (value - (vmStats at: idx)) ]].
	profilerProcess ifNotNil:[
		profilerProcess suspend.
		profilerProcess := nil ]! !


!AndreasSystemProfiler class methodsFor: 'LICENSE' stamp: 'RJT 1/23/2013 15:34'!
LICENSE
	^'Project Squeak

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Copyright (c) 2005-2013, 3D Immersive Collaboration Consulting, LLC., All Rights Reserved

	Redistributions in source code form must reproduce the above
	copyright and this condition.

Licensed under MIT License (MIT)
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'! !

!AndreasSystemProfiler class methodsFor: 'spying' stamp: 'jmv 11/12/2012 15:35'!
spyAllOn: aBlock
	"Spy on all the processes in the system
	
	[1000 timesRepeat: [3.14159 printString. Processor yield]] forkNamed: 'p1'.
	[1000 timesRepeat: [30 factorial. Processor yield]] forkNamed: 'p2'.
	[1000 timesRepeat: [30 factorial. Processor yield]] forkNamed: 'p3'.
	AndreasSystemProfiler spyAllOn: [ (Delay forMilliseconds: 100) wait]

	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	AndreasSystemProfiler spyAllOn: [10000 timesRepeat: [1.23 printString]]
	"
	^self spyOn: aBlock includeAllProcesses: true! !

!AndreasSystemProfiler class methodsFor: 'spying' stamp: 'jmv 11/12/2012 17:31'!
spyForMilliseconds: msecDuration
	"Run the system profiler for the specified number of milliseconds"
	"
	AndreasSystemProfiler spyForMilliseconds: 200
	"
	[ self spyAllOn: [
		(Delay forMilliseconds: msecDuration) wait ]] forkAt: 50! !

!AndreasSystemProfiler class methodsFor: 'spying' stamp: 'jmv 1/2/2013 17:38'!
spyOn: aBlock
	"
	Spy on aBlock, in the current process. Do not include statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	AndreasSystemProfiler spyOn: [10000 timesRepeat: [1.23 printString]]
	"
	"
	AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
	"
	^self spyOn: aBlock includeAllProcesses: false! !

!AndreasSystemProfiler class methodsFor: 'spying' stamp: 'jmv 11/12/2012 15:42'!
spyOn: aBlock includeAllProcesses: aBoolean
	"
	Spy on aBlock, in the current process. Can include or not statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	AndreasSystemProfiler spyOn: [10000 timesRepeat: [1.23 printString]] includeAllProcesses: true
	"
	| profiler |
	profiler := self new.
	profiler observedProcess: (aBoolean ifFalse: [Processor activeProcess]).
	[ ^ profiler spyOn: aBlock]  ensure: [ profiler doReport ].
! !


!QSystemTally methodsFor: 'converting' stamp: 'ar 6/11/2007 22:22'!
asArray
	| link |
	^Array streamContents:[:s|
		link := self.
		[link == nil] whileFalse:[
			s nextPut: link.
			link := link nextLink.
		].
	].! !

!QSystemTally methodsFor: 'converting' stamp: 'ar 6/11/2007 22:34'!
asSortedCollection: aCollection
	"Create a sorted collection from the given input"
	^aCollection asSortedCollection:[:tA :tB| tA tally >= tB tally].! !

!QSystemTally methodsFor: 'report' stamp: 'ar 6/11/2007 22:31'!
bump: hitCount fromSender: senderTally
	"Add this hitCount to the total, and include a reference to the
	sender responsible for the increment"
	self bumpBy: hitCount.
	senders ifNil:[senders := OrderedCollection new].
	senderTally ifNotNil:[senders add: (senderTally copyWithTally: hitCount)]! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 11/12/2012 15:43'!
fullPrintOn: aStream threshold: perCent time: totalTime reportOnly: aProcessOrNil
	| threshold |  
	threshold := (perCent asFloat / 100 * tally) rounded.
	aStream nextPutAll: '**Tree**'; newLine.
	self
		rootPrintOn: aStream
		total: tally
		totalTime: totalTime
		threshold: threshold
		reportOnly: aProcessOrNil.
	aStream nextPut: Character newPage; newLine.
	aStream nextPutAll: '**Leaves**'; newLine.
	self leavesPrintOn: aStream
		threshold: threshold
		time: totalTime.! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 11/12/2012 12:51'!
into: leafDict fromSender: senderTally
	| leafNode |
	leafNode := leafDict at: method
		ifAbsent: [leafDict at: method
			put: ((QSystemTally new class: class method: method)
				process: process) ].
	leafNode bump: tally fromSender: senderTally! !

!QSystemTally methodsFor: 'report' stamp: 'ar 6/11/2007 22:29'!
leavesInto: leafDict fromSender: senderTally
	| rcvrs |
	rcvrs := self sonsOver: 0.
	rcvrs size = 0
		ifTrue: [self into: leafDict fromSender: senderTally]
		ifFalse: [rcvrs do:[:node |
				node isPrimitives
					ifTrue: [node leavesInto: leafDict fromSender: senderTally]
					ifFalse: [node leavesInto: leafDict fromSender: self]]]! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 11/12/2012 12:26'!
leavesPrintOn: aStream threshold: threshold time: totalTime
	| dict |
	dict := IdentityDictionary new: 100.
	self leavesInto: dict fromSender: nil.
	((self asSortedCollection: dict)
		select: [:node | node tally > threshold]) do: [:node |
			node printOn: aStream total: tally totalTime: totalTime tallyExact: false]! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 1/26/2013 17:42'!
printOn: aStream
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally.! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 11/11/2012 22:38'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact

	isExact
		ifTrue: [ 
			| myTally |
			myTally := tally.
			receivers
				ifNotNil: [ receivers asArray do: [ :r | myTally := myTally - r tally ] ].
			aStream
				print: myTally;
				space ]
		ifFalse: [ 
			| percentage |
			percentage := tally asFloat / total * 100.0.
			aStream
				nextPutAll: (percentage printShowingDecimalPlaces: 1);
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded asStringWithCommas;
				nextPutAll: ' ms) '].
	self isPrimitives
		ifTrue: [ 
			aStream
				nextPutAll: 'primitives';
				newLine ]
		ifFalse: [ 
			| className aSelector aClass |
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			className := aClass name contractTo: self maxClassNameSize.
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: (aSelector contractTo: self maxClassPlusSelectorSize - className size);
				newLine ]! !

!QSystemTally methodsFor: 'report' stamp: 'jmv 11/12/2012 15:40'!
rootPrintOn: aStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil

	| groups |
	groups _ (self sonsOver: threshold)
		groupBy: [ :aTally | aTally process ]
		having: [ :g | aProcessOrNil isNil or: [ g anyOne process == aProcessOrNil ]].
	groups do: [ :g |
		| sons |
		sons _ self asSortedCollection: g.
		aStream
			nextPutAll: '--------------------------------';
			newLine;
			nextPutAll: 'Process: ';
			nextPutAll: g anyOne process browserPrintString;
			newLine;
			nextPutAll: '--------------------------------';
			newLine.
		sons do: [ :son |
			son
				treePrintOn: aStream
				tabs: OrderedCollection new
				thisTab: ''
				total: total
				totalTime: totalTime
				tallyExact: false
				orThreshold: threshold].
		aStream newLine ]! !

!QSystemTally methodsFor: 'report' stamp: 'ar 2/27/2008 14:09'!
sonsOver: threshold
	"Answer the sons with tallys over the given threshold"
	receivers ifNil:[^#()].
	^receivers asArray select:[:son | son tally > threshold].! !

!QSystemTally methodsFor: 'report' stamp: 'ar 6/11/2007 22:50'!
treePrintOn: aStream tabs: tabs thisTab: myTab total: total totalTime: totalTime tallyExact: isExact orThreshold: threshold 
	| sons sonTab |
	tabs do: [:tab | aStream nextPutAll: tab].
	tabs size > 0 
		ifTrue:[self printOn: aStream total: total totalTime: totalTime tallyExact: isExact].
	sons := isExact ifTrue: [receivers] ifFalse: [self sonsOver: threshold].
	sons isEmpty ifFalse:[
		tabs addLast: myTab.
		sons := self asSortedCollection: sons.
		1 to: sons size do:[:i | 
			sonTab := i < sons size ifTrue: ['  |'] ifFalse: ['  '].
			(sons at: i) treePrintOn: aStream
						tabs: (tabs size < self maxTabs 
								ifTrue: [tabs]
								ifFalse: [(tabs select: [:x | x = '[']) copyWith: '['])
						thisTab: sonTab
						total: total
						totalTime: totalTime
						tallyExact: isExact
						orThreshold: threshold].
			tabs removeLast]! !

!QSystemTally methodsFor: 'tallying' stamp: 'ar 6/11/2007 21:54'!
bumpBy: count
	"Bump this tally by the specified amount"
	tally := tally + count! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 11/12/2012 17:18'!
tally: context in: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender |

	"Add to this node if appropriate"
	context method == method ifTrue: [
		^self bumpBy: count ].

	"No sender? Add new branch to the tree."
	sender _ context home sender.
	sender ifNil: [
		^(self bumpBy: count) tallyPath: context in: aProcess by: count ].

	"Find the node for the sending context (or add it if necessary)"
	^(self tally: sender in: aProcess by: count) tallyPath: context in: aProcess by: count! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 11/12/2012 12:52'!
tallyMethod: aMethod by: count
	"Called explicitly and needs to decrement receiver's tally count"
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node method == aMethod ifTrue:[^node bumpBy: count].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node bumpBy: count! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 12/26/2012 13:26'!
tallyPath: context in: aProcess by: count
	"Tally the context chain"
	| aMethod t |
	aMethod := context method.
	t := receivers.
	[t == nil] whileFalse:[
		(t method == aMethod and: [t process == aProcess]) ifTrue: [
			^t bumpBy: count ].
		t := t nextLink.
	].
	t := QSystemTally new class: context receiver class method: aMethod.
	t process: aProcess.
	t nextLink: receivers.
	receivers := t.
	^t bumpBy: count! !

!QSystemTally methodsFor: 'initialize' stamp: 'ar 6/11/2007 22:07'!
class: aClass method: aCompiledMethod
	class := aClass.
	method := aCompiledMethod.
	tally := 0.! !

!QSystemTally methodsFor: 'initialize' stamp: 'ar 6/11/2007 22:50'!
primitives: anInteger
	"Make the receiver be a node of unassigned primitives"
	tally := anInteger.
	method := nil. "indicates primitives"! !

!QSystemTally methodsFor: 'copying' stamp: 'jmv 11/12/2012 12:51'!
copyWithTally: hitCount
	^ (QSystemTally new class: class method: method)
		process: process;
		bumpBy: hitCount! !

!QSystemTally methodsFor: 'testing' stamp: 'ar 6/11/2007 22:50'!
isPrimitives
	"Detect pseudo node used to carry tally of local hits"
	^ method == nil! !

!QSystemTally methodsFor: 'accessing' stamp: 'ar 6/11/2007 22:24'!
maxClassNameSize
	"Return the default maximum width of the class name alone"
	^30! !

!QSystemTally methodsFor: 'accessing' stamp: 'ar 6/11/2007 22:24'!
maxClassPlusSelectorSize
	"Return the default maximum width of the class plus selector together (not counting the '>>')"
	^60! !

!QSystemTally methodsFor: 'accessing' stamp: 'jmv 11/12/2012 12:33'!
maxTabs
	"Return the default number of tabs after which leading white space is compressed"
	^150! !

!QSystemTally methodsFor: 'accessing' stamp: 'ar 6/11/2007 21:57'!
method
	"Answer the CompiledMethod associated with this tally"
	^method! !

!QSystemTally methodsFor: 'accessing' stamp: 'jmv 11/12/2012 12:11'!
process
	^process! !

!QSystemTally methodsFor: 'accessing' stamp: 'jmv 11/12/2012 11:39'!
process: aProcess
	process := aProcess! !

!QSystemTally methodsFor: 'accessing' stamp: 'ar 6/11/2007 21:58'!
tally
	"Answer the tally count for this node"
	^tally! !


!QSystemTally class methodsFor: 'LICENSE' stamp: 'RJT 1/23/2013 15:34'!
LICENSE
	^'Project Squeak

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Copyright (c) 2005-2013, 3D Immersive Collaboration Consulting, LLC., All Rights Reserved

	Redistributions in source code form must reproduce the above
	copyright and this condition.

Licensed under MIT License (MIT)
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'! !


!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:08'!
highResClock	
"
Smalltalk highResClock
"
	"Primitive. Answer the value of the high resolution clock if this computer has one.
	Usually, this should be the highest resolution value available, for example on Intel
	it will be the value of the time stamp counter register."
	<primitive: 'primitiveHighResClock'>
	^0! !

!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
interruptChecksPerMSec
"
Smalltalk interruptChecksPerMSec
"
	"Primitive. Answer the number of interrupt checks per milliseconds that we execute
	on this machine. This can be used to adjust the sub-msecs profiler to check (roughly) 
	n times per millisecond."
	<primitive: 'primitiveInterruptChecksPerMSec'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
longRunningPrimitive
	"Primitive. Answer an Array of the current long-running primitive method identified by the
	 heartbeat, and the number of heartbeats it ran for, or nil if none.  To use this, first install a
	 semaphore to wait upon via SystemDictionary>>longRunningPrimitiveSemaphore:"
	<primitive: 'primitiveLongRunningPrimitive'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
longRunningPrimitiveSemaphore: aSemaphoreOrNil
	"Primitive. Install the semaphore to be used for collecting long-running primitives, 
	 or nil if no semaphore should be used.  The semaphore will be signalled once the
	 heartbeat detects a long-running primitive until the result is collected via
	 primitiveLongRunningPrimitive (see SystemDictionary>>longRunningPrimitive)"
	<primitive: 'primitiveLongRunningPrimitiveSemaphore'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profilePrimitive
	"Primitive. Answer the primitive sample by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfilePrimitive'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profileSample
	"Primitive. Answer the last sample taken by the profiler, or nil if the profiler isn't active.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSample'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profileSemaphore: aSemaphore
	"Primitive. Install the semaphore to be used for profiling, 
	or nil if no semaphore should be used.
	See also primitiveProfileStart."
	<primitive: 'primitiveProfileSemaphore'>
	^self primitiveFailed! !

!SystemDictionary methodsFor: '*AndreasProfiler-profiling' stamp: 'jmv 11/12/2012 11:09'!
profileStart: counter
	"Primitive. Begin profiling execution every by using the interrupt check-counter instead of a time-based process (which is limited to timing resolution and triggers off the same signal that many of the processes being profiled trigger off leading to consistently wrong results).
	The argument is the number of interrupt checks (method activations) to let go by before taking a sample. The sample is being stored in the profileSample iVar which can be retrieved by executing primitiveProfileSample. When a sample is taken, it signals the semaphore specified in primitiveProfileSemaphore.
	If the argument is less or equal to zero, it disables profiling."
	<primitive: 'primitiveProfileStart'>
	^self primitiveFailed! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/26/2013 18:25'!
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
	Smalltalk at: #AndreasSystemProfiler ifPresent: [ :enh | ^enh spyAllOn: aBlock ].
	node _ self new.
	node reportOtherProcesses: true.	"Irrelevant in this case. All processes will be reported on their own."
	result _ node spyAllEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Spy Results'
			wrap: false ].
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/26/2013 18:25'!
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
	Smalltalk at: #AndreasSystemProfiler ifPresent: [ :enh | ^enh spyOn: aBlock includeAllProcesses: aBoolean ].
	node _ self new.
	node reportOtherProcesses: aBoolean.
	result _ node spyEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Spy Results'
			wrap: false ].
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/26/2013 18:26'!
spyOn: aBlock toFileNamed: fileName reportOtherProcesses: aBoolean
	"Spy on the evaluation of aBlock. Write the data collected on a file
	named fileName.
	Not supported (yet) by AndreasSystemProfiler"

	| file value node |
	node _ self new.
	node reportOtherProcesses: aBoolean.
	value _ node spyEvery: self defaultPollPeriod on: aBlock.
	file _ FileStream newFileNamed: fileName.
	node report: file.
	file close.
	^value! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/26/2013 18:26'!
spyOnProcess: aProcess forMilliseconds: msecDuration reportOtherProcesses: aBoolean
	"
	Spy on aProcess for a certain amount of time
	| p1 p2 |  
	p1 _ [100000 timesRepeat: [3.14159 printString. Processor yield]] newProcess.  
	p2 _ [100000 timesRepeat: [3.14159 printString. Processor yield]] newProcess.
	p1 resume.
	p2 resume.  
	(Delay forMilliseconds: 100) wait.  
	MessageTally spyOnProcess: p1 forMilliseconds: 1000 reportOtherProcesses: true
	
	Not supported (yet) by AndreasSystemProfiler"
	| node |
	node _ self new.
	node reportOtherProcesses: aBoolean.
	node
		spyEvery: self defaultPollPeriod
		onProcess: aProcess
		forMilliseconds: msecDuration.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Spy Results'
			wrap: false ]! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/26/2013 18:26'!
spyOnProcess: aProcess forMilliseconds: msecDuration toFileNamed: fileName reportOtherProcesses: aBoolean
	"Spy on the evaluation of aProcess. Write the data collected on a file  
	named fileName. Will overwrite fileName
	Not supported (yet) by AndreasSystemProfiler"
	| file node |
	node _ self new.
	node reportOtherProcesses: aBoolean.
	node
		spyEvery: self defaultPollPeriod
		onProcess: aProcess
		forMilliseconds: msecDuration.
	file _ FileStream fileNamed: fileName.
	node report: file.
	file close! !

