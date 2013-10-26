'From Cuis 4.0 of 21 April 2012 [latest update: #1271] on 7 May 2012 at 9:38:15 am'!

!MessageTally class methodsFor: 'spying' stamp: 'jmv 5/7/2012 09:36'!
spyAllOn: aBlock
	"Spy on all the processes in the system
	
	[1000 timesRepeat: [3.14159 printString. Processor yield]] fork.
	[1000 timesRepeat: [20 factorial. Processor yield]] fork.
	[1000 timesRepeat: [20 factorial. Processor yield]] fork.
	MessageTally spyAllOn: [ (Delay forMilliseconds: 100) wait]
	
	"
	| node result |
	node _ self new.
	node reportOtherProcesses: true.	"Irrelevant in this case. All processes will be reported on their own."
	result _ node spyAllEvery: self defaultPollPeriod on: aBlock.
	SystemWindow
		editText: (Workspace withText: (String streamContents: [:s | node report: s]))
		label: 'Spy Results'
		wrap: false.
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 5/7/2012 09:36'!
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
	node _ self new.
	node reportOtherProcesses: aBoolean.
	result _ node spyEvery: self defaultPollPeriod on: aBlock.
	SystemWindow
		editText: (Workspace withText: (String streamContents: [:s | node report: s]))
		label: 'Spy Results'
		wrap: false.
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 5/7/2012 09:37'!
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
	SystemWindow
		editText: (Workspace withText: (String streamContents: [:s | node report: s]))
		label: 'Spy Results'
		wrap: false! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 5/7/2012 09:37'!
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
	SystemWindow
		editText: (Workspace withText: (String streamContents: [ :s |
			s nextPutAll: 'This simulation took ' , totalTime printString, ' seconds.'; newLine.
			treeOption
				ifTrue: [ tallies fullPrintExactOn: s ]
				ifFalse: [ tallies leavesPrintExactOn: s ]]))
		label: 'Spy Results'
		wrap: false! !

