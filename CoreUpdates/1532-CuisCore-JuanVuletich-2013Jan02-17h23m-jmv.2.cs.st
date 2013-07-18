'From Cuis 4.1 of 12 December 2012 [latest update: #1531] on 2 January 2013 at 5:59:49 pm'!

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/2/2013 17:50'!
spyForMilliseconds: msecDuration
	"Run the system profiler for the specified number of milliseconds"
	"
	MessageTally spyForMilliseconds: 200
	"
	[ self spyAllOn: [
		(Delay forMilliseconds: msecDuration) wait ]] forkAt: 50! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/2/2013 17:58'!
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
	Smalltalk at: #QSystemProfiler ifPresent: [ :enh | ^enh spyAllOn: aBlock ].
	node _ self new.
	node reportOtherProcesses: true.	"Irrelevant in this case. All processes will be reported on their own."
	result _ node spyAllEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Spy Results'
			wrap: false ].
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/2/2013 17:54'!
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
	^self spyOn: aBlock reportOtherProcesses: false! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/2/2013 17:58'!
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
	Smalltalk at: #QSystemProfiler ifPresent: [ :enh | ^enh spyOn: aBlock includeAllProcesses: aBoolean ].
	node _ self new.
	node reportOtherProcesses: aBoolean.
	result _ node spyEvery: self defaultPollPeriod on: aBlock.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [:s | node report: s]))
			label: 'Spy Results'
			wrap: false ].
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/2/2013 17:35'!
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
	"
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

!MessageTally class methodsFor: 'spying' stamp: 'jmv 1/2/2013 17:35'!
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
	startTime _ Time millisecondClockValue.
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

	totalTime _ Time millisecondClockValue - startTime // 1000.0 roundTo: 0.01.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [ :s |
				s nextPutAll: 'This simulation took ' , totalTime printString, ' seconds.'; newLine.
				treeOption
					ifTrue: [ tallies fullPrintExactOn: s ]
					ifFalse: [ tallies leavesPrintExactOn: s ]]))
			label: 'Spy Results'
			wrap: false ]! !

