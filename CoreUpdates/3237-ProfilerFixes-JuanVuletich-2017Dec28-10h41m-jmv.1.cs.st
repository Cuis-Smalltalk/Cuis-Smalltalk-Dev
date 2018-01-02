'From Cuis 5.0 of 7 November 2016 [latest update: #3236] on 28 December 2017 at 10:50:51 am'!
!classDefinition: #AndreasSystemProfiler category: #'Tools-Profiling'!
Object subclass: #AndreasSystemProfiler
	instanceVariableNames: 'semaphore ticks profilerProcess tallyRoot vmStats totalTally totalTime startTime ticksPerMSec totalTicks observedProcess '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!Time class methodsFor: 'general inquiries' stamp: 'jmv 12/28/2017 09:52:08'!
highResTimerTicksPerMillisecond
	"
	Time highResTimerTicksPerMillisecond
	"
	| t0 ticks0 ticks1 ticksPerMSec |
	t0 _ Time millisecondClockValue + 2.
	[Time millisecondClockValue >= t0] whileFalse.
	ticks0 := Time primHighResClock.
	[Time millisecondClockValue >= (t0 + 100)] whileFalse.
	ticks1 := Time primHighResClock.
	ticksPerMSec := (ticks1 - ticks0) 
			// (Time millisecondClockValue - t0).
	"Retry if rollover!!"
	^ ticksPerMSec < 0
		ifTrue: [ self highResTimerTicksPerMillisecond ]
		ifFalse: [ ticksPerMSec ]! !


!AndreasSystemProfiler methodsFor: 'testing' stamp: 'jmv 12/28/2017 07:45:21'!
isProfiling
	^ profilerProcess notNil! !


!Time class methodsFor: 'primitives' stamp: 'jmv 12/28/2017 09:53:17'!
primHighResClock	
	"Primitive. Answer the value of the high resolution clock if this computer has one.
	Usually, this should be the highest resolution value available, for example on Intel
	it will be the value of the time stamp counter register.
	Answer is (at least usually) a LargePositiveInteger.
	Implemented on Cog, but not in standard interpreter VMs."
	"
	Time primHighResClock
	On Cog on Linux, OS-X and Windows, this gives sub nano second ticks!!

	Time highResTimerTicksPerMillisecond
	"
	"Not really a clock, but a timer or ticker"

	<primitive: 'primitiveHighResClock'>
	^0! !


!SystemDictionary methodsFor: 'AndreasProfiler-profiling' stamp: 'jmv 12/28/2017 09:45:25'!
profileStart: counter
	"Primitive. Begin profiling execution every by using the interrupt check-counter instead of a time-based process (which is limited to timing resolution and triggers off the same signal that many of the processes being profiled trigger off leading to consistently wrong results).
	The argument is the number of interrupt checks (method activations) to let go by before taking a sample. The sample is being stored in the profileSample iVar which can be retrieved by executing primitiveProfileSample. When a sample is taken, it signals the semaphore specified in primitiveProfileSemaphore.
	If the argument is less or equal to zero, it disables profiling."
	"Not an interrupt check-counter, but #primHighResClock"
	<primitive: 'primitiveProfileStart'>
	^self primitiveFailed! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 12/28/2017 10:04:11'!
printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally

	| aSelector aClass percentage line |
	line _ String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage _ tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 2.
		lineStream nextPutAll: '% ('.
		percentage * totalTime printOn: lineStream fractionDigits: 1.
		lineStream nextPutAll: ' ms) '.
		aSelector _ class selectorAtMethod: method setClass: [ :c | aClass _ c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass == class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector.
		wasInPrimitive ifTrue: [
			self flag: #profilerFriendlyCall:.
			parentTally methodSymbol == #profilerFriendlyCall:
				ifTrue: [
					lineStream nextPutAll: '   -- primitive (reported properly)' ]
				ifFalse: [
					lineStream nextPutAll: '   -- primitive (real sender possibly omitted, see #profilerFriendlyCall:)' ]
			].
		].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 12/28/2017 07:52:24'!
doReport
	"Report the results of this profiler run"
	| report |
	report _ self report.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: report)
			label: 'Spy Results'
			wrap: false ]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 12/28/2017 10:45:37'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks methodForPrimitiveWhileTakingSample parentNode contextToTally |
	totalTally _ 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks _ 0.
	[ true ]
		whileTrue: [
			tallyStart _ Time primHighResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyTicks _ Time primHighResClock - tallyStart.
			"In the extremely unlikely event of high res clock rollover, just ignore this tally"
			tallyTicks > 0 ifTrue: [
				totalTicks _ totalTicks + tallyTicks.
				process _ Smalltalk profileSample.
				methodForPrimitiveWhileTakingSample _ Smalltalk profilePrimitive.
				totalTally _ totalTally + 1.
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
								contextToTally _ process suspendedContext ifNil: [ thisContext ].
								contextToTally method selector == #profilerFriendlyCall: ifFalse: [
									contextToTally _ contextToTally sender ].
								parentNode _ tallyRoot
									tally: contextToTally
									inProcess: process
									by: tallyTicks.
								parentNode
									tallyPrimInMethod: methodForPrimitiveWhileTakingSample by: tallyTicks
								]]]]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 12/28/2017 10:23:54'!
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	semaphore _ Semaphore new.
	"Try to get 10 samples per msec... Not really sure how this parameter is used, nor the meaning and relevance of #interruptChecksPerMSec"
	"ticks _ Time highResTimerTicksPerMillisecond // Smalltalk interruptChecksPerMSec."
	ticks _ Time highResTimerTicksPerMillisecond // 10.
	vmStats _ Smalltalk getVMParameters.
	startTime _ Time localMillisecondClock.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: nil.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 12/28/2017 07:47:24'!
stopProfiling
	"Stop the profiler process"
	Smalltalk profileSemaphore: nil.
	Smalltalk profileStart: 0. "<- profile stops now"
	totalTime _ Time localMillisecondClock - startTime.
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :value | 
		value isNumber ifTrue: [
			vmStats at: idx put: (value - (vmStats at: idx)) ]].
	profilerProcess ifNotNil: [
		profilerProcess terminate.
		profilerProcess _ nil ]! !


!AndreasSystemProfiler class methodsFor: 'spying' stamp: 'jmv 12/28/2017 07:51:36'!
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
	self allInstancesDo: [ :p |
		p isProfiling ifTrue: [
			^ self inform: 'Must first wait for running profiler, or kill it in Process Browser' ]].
	profiler _ self new.
	profiler observedProcess: (aBoolean ifFalse: [Processor activeProcess]).
	[ ^ profiler spyOn: aBlock]  ensure: [ profiler doReport ].
! !

!classDefinition: #AndreasSystemProfiler category: #'Tools-Profiling'!
Object subclass: #AndreasSystemProfiler
	instanceVariableNames: 'semaphore ticks profilerProcess tallyRoot vmStats totalTally totalTime startTime totalTicks observedProcess'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
