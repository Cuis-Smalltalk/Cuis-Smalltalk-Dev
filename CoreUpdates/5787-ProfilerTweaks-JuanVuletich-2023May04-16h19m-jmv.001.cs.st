'From Cuis 6.0 [latest update: #5787] on 4 May 2023 at 4:20:33 pm'!

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 5/4/2023 16:19:53'!
spyOn: aBlock
	"Profile system activity during execution of aBlock."
"	tallyRoot := QSystemTally new class: aBlock receiver class method: aBlock method nesting: 1."
	self startProfiling.
	^aBlock ensure: [ self stopProfiling ]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 5/4/2023 16:20:14'!
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	
	tallyRoot := QSystemTally new class: thisContext receiver class method: thisContext method nesting: 0.
	semaphore := Semaphore new.
	"Try to get 10 samples per msec... Not really sure how this parameter is used, nor the meaning and relevance of #interruptChecksPerMSec"
	"ticks _ Time highResTimerTicksPerMillisecond // Smalltalk interruptChecksPerMSec."
	ticks := Time highResTimerTicksPerMillisecond // 10.
	vmStats := Smalltalk getVMParameters.
	startTime := Time localMillisecondClock.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: nil.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume! !

