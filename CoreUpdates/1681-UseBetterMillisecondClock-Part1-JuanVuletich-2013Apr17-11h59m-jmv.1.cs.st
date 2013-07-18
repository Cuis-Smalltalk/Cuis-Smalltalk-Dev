'From Cuis 4.1 of 12 December 2012 [latest update: #1677] on 17 April 2013 at 12:14:03 pm'!

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 4/17/2013 12:01'!
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	| t0 |
	semaphore := Semaphore new.
	"Run a 100 msec busy loop to figure out the ticks per msec"
	t0 := Time localMillisecondClock + 2.
	[Time localMillisecondClock >= t0] whileFalse.
	ticksPerMSec := Time primHighResClock.
	[Time localMillisecondClock >= (t0 + 100)] whileFalse.
	ticksPerMSec := (Time primHighResClock - ticksPerMSec) 
			// (Time localMillisecondClock - t0).
	"Try to get 10 samples per msec"
	ticks := ticksPerMSec // 10.
	startTime := Time localMillisecondClock.
	vmStats := Smalltalk getVMParameters.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: profilerProcess.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 4/17/2013 12:00'!
stopProfiling
	"Stop the profiler process"
	Smalltalk profileSemaphore: nil.
	Smalltalk profileStart: 0. "<- profile stops now"
	totalTime := Time localMillisecondClock - startTime.
	Smalltalk getVMParameters keysAndValuesDo: [ :idx :value | 
		value isNumber ifTrue: [
			vmStats at: idx put: (value - (vmStats at: idx)) ]].
	profilerProcess ifNotNil:[
		profilerProcess suspend.
		profilerProcess := nil ]! !


!AutoCompleterMorph methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:03'!
stillActive
	lastActivity := Time localMillisecondClock! !

!AutoCompleterMorph methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:03'!
timeOfLastActivity
	^ (Time localMillisecondClock - self lastActivity)! !


!CellStyleEditor methodsFor: 'testing' stamp: 'jmv 4/17/2013 12:03'!
isInCellShiftMode
	^Time localMillisecondClock - lastTime < 700 and: [ markIndex > 1 ]! !

!CellStyleEditor methodsFor: 'typing support' stamp: 'jmv 4/17/2013 12:03'!
dispatchOn: aKeyboardEvent
	| answer |
	storeLastTime _ false.
	answer _ super dispatchOn: aKeyboardEvent.
	lastTime _ storeLastTime
		ifTrue: [Time localMillisecondClock]
		ifFalse: [0].
	^ answer! !


!Form methodsFor: 'transitions' stamp: 'jmv 4/17/2013 12:03'!
wipeImage: otherImage at: topLeft clippingBox: clipBox rectForIndex: rectForIndexBlock

	| i clipRect t rectOrList waitTime |
	i _ 0.
	clipRect _ topLeft extent: otherImage extent.
	clipBox ifNotNil: [clipRect _ clipRect intersect: clipBox].
	[rectOrList _ rectForIndexBlock value: (i _ i + 1).
	 rectOrList == nil]
		whileFalse: [
			t _ Time localMillisecondClock.
			rectOrList asOrderedCollection do: [:r |
				self copyBits: r from: otherImage at: topLeft + r topLeft
					clippingBox: clipRect rule: Form over fillColor: nil].
			Display forceDisplayUpdate.
			waitTime _ 3 - (Time localMillisecondClock - t).
			waitTime > 0 ifTrue:
				["(Delay forMilliseconds: waitTime) wait"]].
! !

!Form methodsFor: 'transitions' stamp: 'jmv 4/17/2013 12:03'!
zoomIn: goingIn orOutTo: otherImage at: topLeft vanishingPoint: vp 
	"Display zoomInTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40.
	Display zoomOutTo: (Form fromDisplay: (40@40 extent: 300@300)) reverse at: 40@40."
	| nSteps j bigR lilR minTime startTime lead |
	nSteps _ 16.
	minTime _ 500.  "milliseconds"
	startTime _ Time localMillisecondClock.
	^ self wipeImage: otherImage at: topLeft clippingBox: nil rectForIndex:
		[:i | "i runs from 1 to nsteps"
		i > nSteps
			ifTrue: [nil "indicates all done"]
			ifFalse:
			["If we are going too fast, delay for a bit"
			lead _ startTime + (i-1*minTime//nSteps) - Time localMillisecondClock.
			lead > 10 ifTrue: [(Delay forMilliseconds: lead) wait].

			"Return an array with the difference rectangles for this step."
			j _ goingIn ifTrue: [i] ifFalse: [nSteps+1-i].
			bigR _ vp - (vp*(j)//nSteps) corner:
				vp + (otherImage extent-vp*(j)//nSteps).
			lilR _ vp - (vp*(j-1)//nSteps) corner:
				vp + (otherImage extent-vp*(j-1)//nSteps).
			bigR areasOutside: lilR]]! !


!Integer class methodsFor: 'prime numbers' stamp: 'jmv 4/17/2013 12:05'!
verbosePrimesUpTo: max do: aBlock
	"Integer verbosePrimesUpTo: SmallInteger maxVal" "<- heh, heh"
	"Compute primes up to max, but be verbose about it"
	| lastTime |
	lastTime := Time localMillisecondClock.
	Utilities informUserDuring:[:bar|
		bar value:'Computing primes...'.
		self primesUpTo: max do:[:prime| | nowTime |
			aBlock value: prime.
			nowTime := Time localMillisecondClock.
			(nowTime - lastTime > 1000) ifTrue:[
				lastTime := nowTime.
				bar value:'Last prime found: ', prime printString]]].! !


!MIDIInputParser methodsFor: 'recording' stamp: 'jmv 4/17/2013 12:05'!
clearBuffers
	"Clear the MIDI record buffers. This should be called at the start of recording or real-time MIDI processing."	

	received _ received species new: 5000.
	rawDataBuffer _ ByteArray new: 1000.
	sysExBuffer _ WriteStream on: (ByteArray new: 100).
	midiPort ifNotNil: [midiPort ensureOpen; flushInput].
	startTime _ Time localMillisecondClock.
	state _ #idle.
! !


!MVCMenuMorph methodsFor: 'invoking' stamp: 'jmv 4/17/2013 12:04'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorld	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorld ]! !


!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 4/17/2013 12:06'!
spyAllEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy all the system processes"

	| myDelay time0 |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time localMillisecondClock.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[true] whileTrue: [
			| startTime observedProcess |
			startTime := Time localMillisecondClock.
			myDelay wait.
			observedProcess := Processor preemptedProcess.
			self
				tally: observedProcess suspendedContext
				in: observedProcess
				"tally can be > 1 if ran a long primitive"
				by: (Time localMillisecondClock - startTime) // millisecs].
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
		time := Time localMillisecondClock - time0]! !

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 4/17/2013 12:06'!
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
	time0 := Time localMillisecondClock.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[ true ] whileTrue: [
			| startTime |
			startTime := Time localMillisecondClock.
			myDelay wait.

			self
				tally: Processor preemptedProcess suspendedContext
				in: (observedProcess == Processor preemptedProcess ifTrue: [observedProcess])
				"tally can be > 1 if ran a long primitive"
				by: (Time localMillisecondClock - startTime) // millisecs].
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
		time := Time localMillisecondClock - time0]! !

!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 4/17/2013 12:06'!
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
					in: (observedProcess == Processor preemptedProcess ifTrue: [ observedProcess ] ifFalse: [nil])
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
	time _ Time localMillisecondClock - time0! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 4/17/2013 12:06'!
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
	startTime _ Time localMillisecondClock.
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

	totalTime _ Time localMillisecondClock - startTime // 1000.0 roundTo: 0.01.
	WorldState addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: (String streamContents: [ :s |
				s nextPutAll: 'This simulation took ' , totalTime printString, ' seconds.'; newLine.
				treeOption
					ifTrue: [ tallies fullPrintExactOn: s ]
					ifFalse: [ tallies leavesPrintExactOn: s ]]))
			label: 'Spy Results'
			wrap: false ]! !


!NetNameResolver class methodsFor: 'lookups' stamp: 'jmv 4/17/2013 12:07'!
addressForName: hostName timeout: secs
	"Look up the given host name and return its address. Return nil if the address is not found in the given number of seconds."
	"NetNameResolver addressForName: 'create.ucsb.edu' timeout: 30"
	"NetNameResolver addressForName: '100000jobs.de' timeout: 30"
	"NetNameResolver addressForName: '1.7.6.4' timeout: 30"
	"NetNameResolver addressForName: '' timeout: 30 (This seems to return nil?)"

	| deadline result |
	self initializeNetwork.
	"check if this is a valid numeric host address (e.g. 1.2.3.4)"
	result _ self addressFromString: hostName.
	result ifNotNil: [^result].

	"Look up a host name, including ones that start with a digit (e.g. 100000jobs.de or squeak.org)"
	deadline _ Time localMillisecondClock + (secs * 1000).
	"Protect the execution of this block, as the ResolverSemaphore is used for both parts of the transaction."
	self resolverMutex
		critical: [
			(self waitForResolverReadyUntil: deadline)
				ifTrue: [
					self primStartLookupOfName: hostName.
					(self waitForCompletionUntil: deadline)
						ifTrue: [result _ self primNameLookupResult]
						ifFalse: [(NameLookupFailure hostName: hostName) signal: 'Could not resolve the server named: ', hostName]]
				ifFalse: [(NameLookupFailure hostName: hostName) signal: 'Could not resolve the server named: ', hostName]].
	^result! !

!NetNameResolver class methodsFor: 'lookups' stamp: 'jmv 4/17/2013 12:07'!
nameForAddress: hostAddress timeout: secs
	"Look up the given host address and return its name. Return nil if the lookup fails or is not completed in the given number of seconds. Depends on the given host address being known to the gateway, which may not be the case for dynamically allocated addresses."
	"NetNameResolver
		nameForAddress: (NetNameResolver addressFromString: '128.111.92.2')
		timeout: 30"

	| deadline |
	self initializeNetwork.
	deadline := Time localMillisecondClock + (secs * 1000).
	"Protect the execution of this block, as the ResolverSemaphore is used for both parts of the transaction."
	^self resolverMutex
		critical: [
			(self waitForResolverReadyUntil: deadline)
				ifTrue: [
					self primStartLookupOfAddress: hostAddress.
					(self waitForCompletionUntil: deadline)
						ifTrue: [self primAddressLookupResult]
						ifFalse: [nil]]
				ifFalse: [nil]].! !

!NetNameResolver class methodsFor: 'private' stamp: 'jmv 4/17/2013 12:07'!
waitForCompletionUntil: deadline
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is ready, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status _ self resolverStatus.
	[(status = ResolverBusy) and:
	 [Time localMillisecondClock < deadline]]
		whileTrue: [
			"wait for resolver to be available"
			ResolverSemaphore waitTimeoutMSecs: (deadline - Time localMillisecondClock).
			status _ self resolverStatus].

	status = ResolverReady
		ifTrue: [^ true]
		ifFalse: [
			status = ResolverBusy ifTrue: [self primAbortLookup].
			^ false].
! !

!NetNameResolver class methodsFor: 'private' stamp: 'jmv 4/17/2013 12:07'!
waitForResolverReadyUntil: deadline
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is not busy, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status _ self resolverStatus.
	status = ResolverUninitialized ifTrue: [^ false].

	[(status = ResolverBusy) and:
	 [Time localMillisecondClock < deadline]]
		whileTrue: [
			"wait for resolver to be available"
			ResolverSemaphore waitTimeoutMSecs: (deadline - Time localMillisecondClock).
			status _ self resolverStatus].

	^ status ~= ResolverBusy
! !


!Pen class methodsFor: 'tablet drawing examples' stamp: 'jmv 4/17/2013 12:08'!
inkBrush
	"Similar to simplePressurePen, but this example uses the average of the recent pen pressure values. The effect is that of a Japanese ink brush that comes up gradually off the paper as the brush is lifted, causing end (and beginning) of each stroke to taper. Requires the optional tablet support primitives which may not be supported on all platforms. Works best in full screen mode. Shift-mouse to exit." 
	"Pen inkBrush"

	| tabletScale historyMSecs pressureHistory pen now currentPressure sum averagePressure p |
	tabletScale _ self tabletScaleFactor.
	historyMSecs _ 120.
	pressureHistory _ OrderedCollection new.
	pen _ Pen newOnForm: Display.
	pen color: Color black.
	Display fillColor: Color white.
	Display restoreAfter: [
		[Sensor shiftPressed and: [Sensor isAnyButtonPressed]] whileFalse: [
			"compute the average pressure over last historyMSecs milliseconds"
			now _ Time localMillisecondClock.
			currentPressure _ (20.0 * Sensor tabletPressure) rounded.
			pressureHistory addLast: (Array with: now with: currentPressure).
			[pressureHistory size > 0 and:
			 [(pressureHistory first first + historyMSecs) < now]]
				whileTrue: [pressureHistory removeFirst].  "prune old entries"
			sum _ pressureHistory inject: 0 into: [:t :e | t + e last].
			averagePressure _ sum // pressureHistory size.

			p _ (Sensor tabletPoint * tabletScale) rounded.
		     averagePressure > 0
				ifTrue: [
					pen roundNib: averagePressure.
					pen goto: p]
				ifFalse: [
					pen place: p]]].
! !


!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testGrabProcessor
	"
	ProcessorTest new testGrabProcessor
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessor
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 25) wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) < 60
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testGrabProcessorOnlyForNoTimeout
	"
	ProcessorTest new testGrabProcessorOnlyForNoTimeout
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 1000
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 25) wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: (updates = tries)
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: (observations = tries)
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) <60
		description: 'The low priority process was not preempted right after #grabProcessor'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testGrabProcessorOnlyForTimeout
	"
	1 to: 40 do: [ :i | i print. ProcessorTest new testGrabProcessorOnlyForTimeout ]
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait consistentCount inconsistentCount |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	consistentCount _ 0.
	inconsistentCount _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 25 							"About half the time should preempt it before it finishes"
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 15) wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		"If it needs time to finish, at low priority, allow for it."
		[ consistentState ] whileFalse: [ (Delay forMilliseconds: 2) wait ].
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted after the tiemout'.
	self
		assert: consistentCount > 3
		description: 'It seems the low priority process finished always before the timeout'.
	self
		assert: inconsistentCount > 5
		description: 'It seems the low priority process was not preempted at the timeout'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 4/17/2013 12:08'!
testValueUnpreemptively
	"
	ProcessorTest new testValueUnpreemptively
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] valueUnpreemptively
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time localMillisecondClock.
		(Delay forMilliseconds: 25) wait.
		waited _ Time localMillisecondClock - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) < 60
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !


!QueueSound methodsFor: 'initialization' stamp: 'jmv 4/17/2013 12:08'!
initialize
	super initialize.
	sounds _ SharedQueue new.
	done _ false.
	startTime _ Time localMillisecondClock! !

!QueueSound methodsFor: 'sound generation' stamp: 'jmv 4/17/2013 12:08'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Play a collection of sounds in sequence."

	| finalIndex i remaining count rate |
	self currentSound ifNil: [^ self].  "already done"
	self startTime > Time localMillisecondClock ifTrue: [^ self].
	rate _ self samplingRate.
	finalIndex _ (startIndex + n) - 1.
	i _ startIndex.
	[i <= finalIndex] whileTrue: [
		[
			self currentSound ifNil: [^ self].
			(remaining _ self currentSound samplesRemaining) <= 0]
				whileTrue: [self currentSound: self nextSound].
		count _ (finalIndex - i) + 1.
		remaining < count ifTrue: [count _ remaining].
		self currentSound mixSampleCount: count into: aSoundBuffer startingAt: i leftVol: leftVol rightVol: rightVol.
		i _ i + count]! !


!ScorePlayer methodsFor: 'midi output' stamp: 'jmv 4/17/2013 12:09'!
midiPlayLoop

	| mSecsPerStep tStart mSecs |
	mSecsPerStep _ 5.
	[done] whileFalse: [
		tStart _ Time localMillisecondClock.
		self processAllAtTick: ticksSinceStart asInteger.
		(Delay forMilliseconds: mSecsPerStep) wait.
		mSecs _ Time localMillisecondClock - tStart.
		ticksSinceStart _ ticksSinceStart + (mSecs asFloat / (1000.0 * secsPerTick))]! !


!StreamingMonoSound methodsFor: 'playing' stamp: 'jmv 4/17/2013 12:11'!
millisecondsSinceStart
	"Answer the number of milliseconds of this sound started playing."

	| mSecs |
	(stream isNil or: [stream closed]) ifTrue: [^ 0].
	mSecs _ self currentSampleIndex * 1000 // streamSamplingRate.
	(self isPlaying and: [lastBufferMSecs > 0]) ifTrue: [
		"adjust mSecs by the milliseconds since the last buffer"
		mutex critical: [
			mSecs _ self currentSampleIndex * 1000 // streamSamplingRate.
			mSecs _ mSecs + (Time localMillisecondClock - lastBufferMSecs)]].
	^ mSecs + 350 - (2 * SoundPlayer bufferMSecs)
! !

!StreamingMonoSound methodsFor: 'playing' stamp: 'jmv 4/17/2013 12:11'!
playSampleCount: n into: aSoundBuffer startingAt: startIndex
	"Mix the next n samples of this sound into the given buffer starting at the given index"

	self repeat ifTrue: [  "loop if necessary"
		(totalSamples - self currentSampleIndex) < n ifTrue: [self startOver]].

	mutex critical: [
		lastBufferMSecs _ Time localMillisecondClock.
		self loadBuffersForSampleCount: (n * streamSamplingRate) // SoundPlayer samplingRate.
		mixer playSampleCount: n into: aSoundBuffer startingAt: startIndex].
! !


!TestValueWithinFix methodsFor: 'tests' stamp: 'jmv 4/17/2013 12:11'!
testValueWithinNonLocalReturnFixReal
	"self run: #testValueWithinNonLocalReturnFixReal"
	"The real test for the fix is just as obscure as the original problem"
	| startTime |
	self valueWithinNonLocalReturn.
	startTime := Time localMillisecondClock.
	[[] repeat] valueWithin: 100 milliSeconds onTimeout:[ | deltaTime |
		"This *should* timeout after 100 msecs but the pending process from
		the previous invokation will signal timeout after 20 msecs already
		which will in turn cut this invokation short."
		deltaTime := Time localMillisecondClock - startTime.
		self deny: deltaTime < 90.
	].
! !


!Time class methodsFor: 'general inquiries' stamp: 'jmv 4/17/2013 12:02'!
millisecondClockValue
	"Answer the number of milliseconds since the millisecond clock was last reset or rolled over.
	Answer 0 if the primitive fails.
	
	Note: Prefer #localMillisecondClock, that doesn't rollover."

	^ self primMillisecondClock! !

