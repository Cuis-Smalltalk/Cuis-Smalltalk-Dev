'From Cuis 4.1 of 12 December 2012 [latest update: #1519] on 21 December 2012 at 1:56:29 pm'!

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 12/21/2012 11:56'!
spyAllEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy all the system processes"

	| myDelay time0 |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time millisecondClockValue.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[true] whileTrue: [
			| startTime observedProcess |
			startTime := Time millisecondClockValue.
			myDelay wait.
			observedProcess := Processor preemptedProcess.
			self
				tally: observedProcess suspendedContext
				in: observedProcess
				"tally can be > 1 if ran a long primitive"
				by: (Time millisecondClockValue - startTime) // millisecs].
		nil] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	^ aBlock ensure: [
		"cancel the probe and return the value"
		"Could have already been terminated. See #terminateTimerProcess"
		Timer ifNotNil: [
			Timer terminate.
			Timer _ nil ].
		"Collect gc statistics"
		Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
			gcVal isNumber ifTrue: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time millisecondClockValue - time0]! !

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 12/21/2012 11:56'!
spyEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy only on the active process (in which aBlock is run)"

	| myDelay time0 observedProcess |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	observedProcess _ Processor activeProcess.
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time millisecondClockValue.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[ true ] whileTrue: [
			| startTime |
			startTime := Time millisecondClockValue.
			myDelay wait.

			self
				tally: Processor preemptedProcess suspendedContext
				in: (observedProcess == Processor preemptedProcess ifTrue: [observedProcess])
				"tally can be > 1 if ran a long primitive"
				by: (Time millisecondClockValue - startTime) // millisecs].
		nil] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	^ aBlock ensure: [
		"cancel the probe and return the value"
		"Could have already been terminated. See #terminateTimerProcess"
		Timer ifNotNil: [
			Timer terminate.
			Timer _ nil ].
		"Collect gc statistics"
		Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
			gcVal isNumber ifTrue: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time millisecondClockValue - time0]! !

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 12/21/2012 11:57'!
spyEvery: millisecs onProcess: aProcess forMilliseconds: msecDuration
	"Create a spy and spy on the given process at the specified rate."
	| myDelay time0 endTime observedProcess sem |
	(aProcess isKindOf: Process)
		ifFalse: [self error: 'spy needs a Process here'].
	self class: aProcess suspendedContext receiver class method: aProcess suspendedContext method.
	"set up the probe"
	observedProcess _ aProcess.
	myDelay _ Delay forMilliseconds: millisecs.
	time0 _ Time millisecondClockValue.
	endTime _ time0 + msecDuration.
	sem _ Semaphore new.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
			[
				| startTime |
				startTime _ Time millisecondClockValue.
				myDelay wait.
				self
					tally: Processor preemptedProcess suspendedContext
					in: (observedProcess == Processor preemptedProcess ifTrue: [ observedProcess ] ifFalse: [nil])
					"tally can be > 1 if ran a long primitive"
					by: (Time millisecondClockValue - startTime) // millisecs.
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
	time _ Time millisecondClockValue - time0! !

