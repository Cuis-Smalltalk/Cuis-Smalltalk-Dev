'From Cuis 4.2 of 25 July 2013 [latest update: #2384] on 25 June 2015 at 10:52:01.701776 am'!

!TimeProfileBrowser class methodsFor: 'instance creation' stamp: 'jmv 6/25/2015 10:51'!
spyOnProcess: aProcess forMilliseconds: msecDuration 
	"Run aProcess for msecDuration milliseconds, then open a TimeProfileBrowser on the results."

	"
	| p |  
	p _ [100000 timesRepeat: [3.14159 printString]] newProcess.
	p resume.  
	(Delay forMilliseconds: 100) wait.  
	TimeProfileBrowser spyOnProcess: p forMilliseconds: 1000
	"

	| inst |
	inst _ self new.
	inst
		runProcess: aProcess
		forMilliseconds: msecDuration
		pollingEvery: MessageTally defaultPollPeriod.
	MessageSetWindow
		open: inst
		label: (String streamContents: [ :s |
			s nextPutAll: 'Time Profile for '; print: msecDuration; nextPutAll: ' msec' ]).
	^ inst! !


!MessageTally methodsFor: 'initialization' stamp: 'jmv 6/25/2015 10:41'!
spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration
	"Create a spy and spy on the given process at the specified rate."
	| myDelay time0 endTime observedProcess sem |
	(aProcess isKindOf: Process)
		ifFalse: [self error: 'spy needs a Process here'].
	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.
	"set up the probe"
	observedProcess _ aProcess.
	myDelay _ Delay forMilliseconds: millisecs.
	time0 _ Time localMillisecondClock.
	endTime _ time0 + msecDuration.
	sem _ Semaphore new.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
			[
				| startTime |
				startTime _ Time localMillisecondClock.
				myDelay wait.
				self
					tally: Processor preemptedProcess suspendedContext
					in: (observedProcess isTheLastPreempted ifTrue: [ observedProcess ])
					"tally can be > 1 if ran a long primitive"
					by: (Time localMillisecondClock - startTime) // millisecs.
				startTime < endTime
			] whileTrue.
			sem signal.
		] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	"activate the probe and wait for it to finish"
	sem wait.
	Timer _ nil.
	"Collect gc statistics"
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
		gcVal isNumber ifTrue: [
			gcStats at: idx put: (gcVal - (gcStats at: idx))]].
	time _ Time localMillisecondClock - time0! !


!Morph methodsFor: 'initialization' stamp: 'jmv 6/25/2015 10:50'!
openInWorld

	self runningWorld
		ifNil: [ WorldState addDeferredUIMessage: [ self openInWorld ]]
		ifNotNil: [ :w | self openInWorld: w ]! !

