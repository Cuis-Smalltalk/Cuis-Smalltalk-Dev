"
AndreasSystemProfiler uses sub-msec VM supported PC sampling.

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Released by Ron, Julie and David

Some technical details from Eliot Miranda ( http://forum.world.st/AndreasSystemProfiler-Released-MIT-tp4664958p4665182.html ):

Both AndreasSystemProfiler and MessageTally are periodic sampling profilers.  The essential difference between AndreasSystemProfiler and MessageTally is in how the current method is sampled.

MessageTally is driven from a high-priority process in a loop waiting on a delay.  When the delay fires the lower-priority process being profiled is interrupted, its stack is walked to determine the methods along the call chain, and that data is recorded.  But since the sampling occurs when the high-priority process preempts the lower-priority process, a sample is only taken at a preemption point.  In particular, primitives are *not* profiled because they are not suspension points.  A process can only be suspended on method activation (a non-primitive method activation, or primitive failure) or on backward branch.  The cost of primitives is charged to a caller and is inferred by subtracting the cost of children of the caller from the caller itself (subtracting the number of samples in children of the caller form the number of samples in the caller itself).  

Another problem is that using the clock that underlies Delay, which is typically the clock used by processes being profiled, causes sampling errors due to the sampling and sampled processes cohering.  Delays are limited in resolution (at best 1 millisecond) so if the profiled process waits on a delay it'll fire immediately after the profiling process (because the profiling process is at higher priority) and so the sampling process may only ever see the sampled process in a wait state.

If MessageTally is used to profile multiple processes then a third problem is that if a primitive causes a process switch then its cost will end up being charged to the process switched-to, not switched from.  This is again because sampling can only occur after a primitive has completed (successfully or not).

AndreasSystemProfiler is driven from a high-priority process in a loop waiting on a Semaphore known to the VM.  The profiling process uses a primitive to schedule a sample some number of ticks of the VM's high-performance clock in the future.  When the time is reached the VM samples the current method and the current process, *before any process preemption takes place*, and independently of the standard clock, and signals the semaphore.  The profiling process then collects the method,process pair via primitives.  So AndreasSystemProfiler provides much more accurate results.

That said there are still limitations with primitives and Cog.  Currently Cog only samples "interpreter" primitives.  Those primitives it implements in machine code (integer and float arithmetic, closure evaluation, at:, identityHash) are not sampled and won't show up; they will be charged to the calling method.  This is fixable, since Cog actually compiles the sampling direct into interpreter primitive invocation when profiling is in effect and not at other times, but sampling could be a significant cost in these simple and performance-critical primitives.
"
Class {
	#name : 'AndreasSystemProfiler',
	#superclass : 'Object',
	#instVars : [
		'semaphore',
		'ticks',
		'profilerProcess',
		'tallyRoot',
		'vmStats',
		'totalTally',
		'totalTime',
		'startTime',
		'totalTicks',
		'observedProcess'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'LICENSE' }
AndreasSystemProfiler class >> LICENSE [
	^'Project Squeak

	In Memory of Andreas Raab.  Author, Friend, Colleague. 	http://forum.world.st/In-Memory-of-Andreas-Raab-td4663424.html
	Copyright (c) 2005-2013, 3D Immersive Collaboration Consulting, LLC., All Rights Reserved

	Redistributions in source code form must reproduce the above
	copyright and this condition.

Licensed under MIT License (MIT)
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'
]

{ #category : 'testing' }
AndreasSystemProfiler class >> canWork [
	"
	AndreasSystemProfiler canWork
	"
	Time primHighResClock = 0 ifTrue: [ ^false ].
	[
		Smalltalk profileSemaphore: nil.
		Smalltalk profileStart: 0 ] on: Error do: [ ^false ].
	^true
]

{ #category : 'instance creation' }
AndreasSystemProfiler class >> new [
	self canWork ifFalse: [
		self error: 'AndreasSystemProfiler primitives not present in this VM' ].
	^super new
]

{ #category : 'spying' }
AndreasSystemProfiler class >> spyAllOn: aBlock [
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
	^self spyOn: aBlock includeAllProcesses: true
]

{ #category : 'spying' }
AndreasSystemProfiler class >> spyForMilliseconds: msecDuration [
	"Run the system profiler for the specified number of milliseconds"
	"
	AndreasSystemProfiler spyForMilliseconds: 200
	"
	[ self spyAllOn: [
		(Delay forMilliseconds: msecDuration) wait ]] forkAt: 50
]

{ #category : 'spying' }
AndreasSystemProfiler class >> spyOn: aBlock [
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
	^self spyOn: aBlock includeAllProcesses: false
]

{ #category : 'spying' }
AndreasSystemProfiler class >> spyOn: aBlock includeAllProcesses: aBoolean [
	"
	Spy on aBlock, in the current process. Can include or not statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	AndreasSystemProfiler spyOn: [10000 timesRepeat: [1.23 printString]] includeAllProcesses: true
	"
	| profiler |
	self allInstancesDo: [ :p |
		p isProfiling ifTrue: [
			^ self inform: 'Must first wait for running profiler, or kill it in Process Browser' ]].
	profiler := self new.
	profiler observedProcess: (aBoolean ifFalse: [Processor activeProcess]).
	[ ^ profiler spyOn: aBlock]  ensure: [ profiler doReport ].

]

{ #category : 'reporting' }
AndreasSystemProfiler >> doReport [
	"Report the results of this profiler run"
	| report |
	report := self report.
	UISupervisor whenUIinSafeState: [
		SystemWindow
			editText: (Workspace withText: report)
			label: 'Spy Results'
			wrap: false ]
]

{ #category : 'testing' }
AndreasSystemProfiler >> isProfiling [
	^ profilerProcess notNil
]

{ #category : 'accessing' }
AndreasSystemProfiler >> observedProcess: aProcess [
	observedProcess := aProcess
]

{ #category : 'reporting' }
AndreasSystemProfiler >> report [
	"Answer a report, with cutoff percentage of each element of the tree"
	^String streamContents: [ :textStream |
		self
			reportTextOn: textStream
			linesOn: (DummyStream on: nil)
			talliesOn: (DummyStream on: nil) ]
]

{ #category : 'reporting' }
AndreasSystemProfiler >> reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream [
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows line |
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

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line := '**Memory**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line := ' old ', oldSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line := ' young ', (youngSpaceEnd - oldSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line :=  ' used ', youngSpaceEnd printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line :=  ' free ', (memoryEnd - youngSpaceEnd) printStringWithCommas, ' bytes'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.

	line := '**GCs**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line := ' full ', fullGCs printString,' totalling ', fullGCTime printStringWithCommas, 'ms (', (100.0 * fullGCTime / upTime) rounded printString, '% uptime)'.
	fullGCs = 0 ifFalse: [
		line := line, ', avg ', (1.0 * fullGCTime / fullGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line := ' incr ', incrGCs printString, ' totalling ', incrGCTime printStringWithCommas, 'ms (', (100.0 * incrGCTime / upTime) rounded printString, '% uptime)'.
	incrGCs = 0 ifFalse: [
		line := line, ', avg ', (1.0 * incrGCTime / incrGCs) rounded printString, 'ms'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line := ' tenures ', tenureCount printStringWithCommas.
	tenureCount = 0 ifFalse: [
		line := line, ' (avg ', (1.0 * incrGCs / tenureCount) asInteger printString, ' GCs/tenure)'].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.

	line := ' root table ', rootOverflows printStringWithCommas, ' overflows'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
]

{ #category : 'reporting' }
AndreasSystemProfiler >> reportProcessStatsOn: textStream linesOn: linesStream talliesOn: talliesStream [
	| totalSwitches pageOverflows pageDivorces actualSwitches line |
	vmStats size >= 61 ifFalse:[^self]. "don't try this on the closure VM"
	totalSwitches := vmStats at: 56.
	actualSwitches := totalSwitches - (2*totalTally). "exclude to/from profiler"
	pageOverflows := vmStats at: 60.
	pageDivorces := vmStats at: 61.

	textStream newLine.
	linesStream nextPut: ''.
	talliesStream nextPut: nil.
	
	line := '**Processes**'.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line := '	Total process switches: ', totalSwitches printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line := '	Without Profiler: ', actualSwitches printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line := '	Stack page overflows: ', pageOverflows printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
	
	line := '	Stack page divorces: ', pageDivorces printString.
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStream nextPut: nil.
]

{ #category : 'reporting' }
AndreasSystemProfiler >> reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream [
	"Print a report, with cutoff percentage of each element of the tree 
	(leaves, roots, tree)=2, on the stream, strm."

	self reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: 0.2
]

{ #category : 'reporting' }
AndreasSystemProfiler >> reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: threshold [
	| line |
	tallyRoot tally = 0
		ifTrue: [
			line := ' - no tallies obtained'.
			textStream nextPutAll: line; newLine.
			linesStream nextPut: line.
			talliesStream nextPut: nil ]
		ifFalse: [
			line := 'Reporting - ' , totalTally printStringWithCommas, ' tallies, ' , totalTime printStringWithCommas , ' msec.'.
			textStream
				nextPutAll: line; newLine;
				newLine.
			linesStream nextPut: line; nextPut: ''.
			talliesStream nextPut: nil; nextPut: nil.
			tallyRoot
				fullPrintOn: textStream
				linesOn: linesStream
				talliesOn: talliesStream 
				threshold: threshold
				time: totalTime
				reportOnly: observedProcess ].
	totalTime isZero ifFalse: [
		self reportGCStatsOn: textStream linesOn: linesStream talliesOn: talliesStream.
		self reportProcessStatsOn: textStream linesOn: linesStream talliesOn: talliesStream ]
]

{ #category : 'profiling' }
AndreasSystemProfiler >> runProfilerProcess [
	"Run the profiler process"

	| process tallyStart tallyTicks methodForPrimitiveWhileTakingSample parentNode contextToTally |
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	[ true ]
		whileTrue: [
			tallyStart := Time primHighResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyTicks := Time primHighResClock - tallyStart.
			"In the extremely unlikely event of high res clock rollover, just ignore this tally"
			tallyTicks > 0 ifTrue: [
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
									AndreasSystemProfiler is more accurate because it uses VM support to tell it which primitive was running when it took a sample. 
									MessageTally simply ascribes a primitive's cost to the method at the next suspension point, which, in some contexts, 
									can yield wildly misleading results."
								"The problem is that knowing just the primitive and the process doesn't give us the complete call stack.
								So, this is, in a sense, approximate."
								"
									AndreasSystemProfiler spyOn: [
								 	       [ #((1 2 3)) do: [ :each |
 									               each findLast: [ :ea |
									                        ea squared = ea ] ] ] bench ].
								Without asking #sender to the context, for this example
									AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
								gave:
									  |  2.9% (7 ms) (Number>>#raisedToInteger:)
									  |    2.2% (5 ms) (Float>>#timesTwoPower: )
								but #raisedToInteger: does NOT send #timesTwoPower:
								Approach taken: Add to parent node, but print with a note that specifies this is primitives, and maybe parent node is missing.
								Additionally, add a note, suggesting #profilerFriendlyCall:

								For example
									AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
										Here, the real parent node is missing.

									AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 1000]].
										Here, the proper tree is shown.

								See profilerFriendlyCall:
								"
								contextToTally := process suspendedContext ifNil: [ thisContext ].
								contextToTally method selector == #profilerFriendlyCall: ifFalse: [
									contextToTally := contextToTally sender ].
								parentNode := tallyRoot
									tally: contextToTally
									inProcess: process
									by: tallyTicks.
								parentNode
									tallyPrimInMethod: methodForPrimitiveWhileTakingSample by: tallyTicks
								]]]]
]

{ #category : 'profiling' }
AndreasSystemProfiler >> spyOn: aBlock [
	"Profile system activity during execution of aBlock."
"	tallyRoot := QSystemTally new class: aBlock receiver class method: aBlock method nesting: 1."
	self startProfiling.
	^aBlock ensure: [ self stopProfiling ]
]

{ #category : 'profiling' }
AndreasSystemProfiler >> startProfiling [
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	
	tallyRoot := QSystemTally new class: thisContext receiver class method: thisContext method nesting: 0.
	semaphore := Semaphore new.
	"Try to get 10 samples per msec... Not really sure how this parameter is used, nor the meaning and relevance of #interruptChecksPerMSec"
	"ticks := Time highResTimerTicksPerMillisecond // Smalltalk interruptChecksPerMSec."
	ticks := Time highResTimerTicksPerMillisecond // 10.
	vmStats := Smalltalk getVMParameters.
	startTime := Time localMillisecondClock.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: nil.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume
]

{ #category : 'profiling' }
AndreasSystemProfiler >> stopProfiling [
	"Stop the profiler process"
	Smalltalk profileSemaphore: nil.
	Smalltalk profileStart: 0. "<- profile stops now"
	totalTime := Time localMillisecondClock - startTime.
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :value | 
		value isNumber ifTrue: [
			vmStats at: idx put: (value - (vmStats at: idx)) ]].
	profilerProcess ifNotNil: [
		profilerProcess terminate.
		profilerProcess := nil ]
]

{ #category : 'accessing result' }
AndreasSystemProfiler >> totalMilliseconds [
	^ totalTime
]

{ #category : 'accessing result' }
AndreasSystemProfiler >> totalMillisecondsFor: aSelector [
	"Answer the sum of the time spent in all appareances in aSelector in the tree"
	| totalTallies |
	totalTallies := 0.
	tallyRoot
		treeDFSDo: [ :eachTally :parentTally |
			(eachTally methodSymbol == aSelector and: [ eachTally blockNestingCount = 0]) ifTrue: [
				totalTallies := eachTally tally + totalTallies ]]
		afterChildrenDo: [ :eachTally | ]
		threshold: 0.0
		parent: nil.
	^ totalTallies asFloat / tallyRoot tally * totalTime
]
