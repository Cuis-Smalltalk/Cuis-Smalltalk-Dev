'From Cuis 4.2 of 25 July 2013 [latest update: #2383] on 22 June 2015 at 6:58:55.444905 pm'!

!TimeProfileBrowser class methodsFor: 'instance creation' stamp: 'pb 6/22/2015 18:58'!
spyOnProcess: aProcess forMilliseconds: msecDuration 
	"Run aProcess for msecDuration milliseconds, then open a TimeProfileBrowser on the results."

	"| p |  
	p _ [100000 timesRepeat: [3.14159 printString]] fork.  
	(Delay forMilliseconds: 100) wait.  
	TimeProfileBrowser spyOnProcess: p forMilliseconds: 1000"

	| inst |
	inst := self new.
	inst runProcess: aProcess forMilliseconds: msecDuration pollingEvery: MessageTally defaultPollPeriod.
	WorldState addDeferredUIMessage: [
		MessageSetWindow
			open: inst
			label: (String streamContents: [ :s | s nextPutAll: 'Time Profile for '; print: msecDuration; nextPutAll: ' msec' ])].
	^ inst! !


!MessageTally methodsFor: 'initialization' stamp: 'pb 6/22/2015 18:57'!
spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration
	"Create a spy and spy on the given process at the specified rate."
	| myDelay time0 endTime observedProcess sem |
	(aProcess isKindOf: Process)
		ifFalse: [self error: 'spy needs a Process here'].
	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.
	"set up the probe"
	observedProcess := aProcess.
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time localMillisecondClock.
	endTime := time0 + msecDuration.
	sem := Semaphore new.
	gcStats := Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer := [
			[
				| startTime |
				startTime := Time localMillisecondClock.
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
	"Collect gc statistics"
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
		gcVal isNumber ifTrue: [
			gcStats at: idx put: (gcVal - (gcStats at: idx))]].
	time := Time localMillisecondClock - time0.
	self class terminateTimerProcess.! !

