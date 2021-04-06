'From Cuis 5.0 [latest update: #4558] on 5 April 2021 at 3:39:29 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 4/5/2021 15:37:00'!
userInterrupt: aProcess
	"Create a Notifier on the active scheduling process with the given label."
	| process  |
	process _ aProcess.

	"Only debug aProcess if its priority is >= UIProcess' priority"
	UIProcess ifNotNil: [
		process priority < UIProcess priority ifTrue: [
			process _ UIProcess ]].

	Debugger interruptProcess: process label: 'User Interrupt'! !


!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 4/5/2021 15:30:04'!
dumpPigStackOn: aStream
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process depth stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	"UISupervisor whenUIinSafeState: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; newLine.
	self dumpTally: tally on: aStream.
	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.
	process _ tally sortedCounts first value.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringFull: false);
		newLine.
	depth _ 20.
	stack _ process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: depth]
		ifFalse: [suspendedContext _ process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: depth]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; newLine]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream newLine ]].
	^process! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 4/5/2021 15:37:58'!
handleUserInterrupt
	| p |
	p _ Utilities reportCPUandRAM.
	Preferences cmdDotEnabled ifTrue: [
		[ UISupervisor userInterrupt: p ] fork]! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 4/5/2021 15:39:00'!
reportCPUandRAM
	"Write several text files with useful analysis for profiling purposes.
	Overwrites any existing report.
	Utilities reportCPUandRAM
	"	

	| profiler pig |
	
	"VM statistics (Memory use and GC, mainly)"
	'MemoryStats.txt' asFileEntry forceWriteStreamDo: [ :stream |
		stream nextPutAll: self vmStatisticsReportString ].

	"Process list"
	'ProcessList.txt' asFileEntry forceWriteStreamDo: [ :stream |
		ProcessBrowser new processNameList 
			do: [ :each | 
				stream nextPutAll: each; newLine ]].

	"Process taking most CPU"
	'ThePig.txt' asFileEntry forceWriteStreamDo: [ :stream |
		pig _ ProcessBrowser dumpPigStackOn: stream ].

	"Tally of all processes"
	AndreasSystemProfiler canWork ifTrue: [
		'FullTally.txt' asFileEntry forceWriteStreamDo: [ :stream |
			profiler _ AndreasSystemProfiler new.
			profiler spyOn: [ (Delay forMilliseconds: 1000) wait ].
			profiler
				reportTextOn: stream
				linesOn: (DummyStream on: nil)
				talliesOn: (DummyStream on: nil) ]].

	"Memory Analysis"
	'MemoryAnalysis.txt' asFileEntry forceWriteStreamDo: [ :stream |
		SpaceTally new printSpaceAnalysis: 1 on: stream ].

	^pig! !

!methodRemoval: UISupervisor class #userInterrupt stamp: 'jmv 4/5/2021 15:38:26'!
UISupervisor class removeSelector: #userInterrupt!
