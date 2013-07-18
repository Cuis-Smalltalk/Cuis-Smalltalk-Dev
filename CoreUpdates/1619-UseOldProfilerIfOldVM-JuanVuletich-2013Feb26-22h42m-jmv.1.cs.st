'From Cuis 4.1 of 12 December 2012 [latest update: #1616] on 26 February 2013 at 10:43:07 pm'!

!AndreasSystemProfiler class methodsFor: 'testing' stamp: 'jmv 2/26/2013 22:36'!
canWork
	"
	AndreasSystemProfiler canWork
	"
	Smalltalk highResClock = 0 ifTrue: [ ^false ].
	[
		Smalltalk profileSemaphore: nil.
		Smalltalk profileStart: 0 ] on: Error do: [ ^false ].
	^true! !


!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/26/2013 22:30'!
spyOn: aBlock
	"Profile system activity during execution of aBlock."
	tallyRoot := QSystemTally new class: aBlock receiver class method: aBlock method.
	self startProfiling.
	^aBlock ensure: [ self stopProfiling ]! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 2/26/2013 22:37'!
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
	AndreasSystemProfiler canWork ifTrue: [
		^AndreasSystemProfiler spyAllOn: aBlock ].
	node _ self new.
	node reportOtherProcesses: true.	"Irrelevant in this case. All processes will be reported on their own."
	result _ node spyAllEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Classic MessageTally Spy Results (AndreasSystemProfiler primitives not present in this VM)'
			wrap: false ].
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 2/26/2013 22:38'!
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
	AndreasSystemProfiler canWork ifTrue: [
		^AndreasSystemProfiler spyOn: aBlock includeAllProcesses: aBoolean ].
	node _ self new.
	node reportOtherProcesses: aBoolean.
	result _ node spyEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Classic MessageTally Spy Results (AndreasSystemProfiler primitives not present in this VM)'
			wrap: false ].
	^ result! !

