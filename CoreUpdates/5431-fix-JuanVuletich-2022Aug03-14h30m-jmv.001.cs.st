'From Cuis 6.0 [latest update: #5432] on 3 August 2022 at 2:33:54 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 8/3/2022 13:10:31'!
millisecondsSinceLastCycle
	"
	self runningWorld millisecondsSinceLastCycle
	"
	^Time localMillisecondClock - lastCycleTime! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/3/2022 13:58:47'!
handleUserInterrupt
	| processToInterrupt |
	"If the UI process is not ready to run, the UI was not responsive, and most likely that's why the user pressed
	the interrupt key. Make GUI responsive again, and let them debug the process that made UI not responsive."
	processToInterrupt _ UISupervisor newUIProcessIfCurrentCantRun.
	processToInterrupt ifNil: [
		"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
		processToInterrupt _ Sensor shiftPressed | (Preferences at: #cmdDotInterruptTakesStatistics)
			ifTrue: [Utilities reportCPUandRAM]
			ifFalse: [Utilities processTakingMostCPU]].
	(Preferences at: #cmdDotEnabled) ifTrue: [
		"The background process can't be interrupted, or Cuis will likely crash."
		processToInterrupt == Processor backgroundProcess ifTrue: [
			'Can not interrupt backgroundProcess' print.
			^self ].
		(processToInterrupt name beginsWith: '[system]') ifTrue: [
			 ('Process {', processToInterrupt printString, '} is critical for system stability. Can not interrupt it.') print.
			^self ].
		[
			EventSensor install.
			processToInterrupt isTerminated
				ifTrue: [ ('Process {', processToInterrupt printString, '} isTerminated. Can not interrupt it.') print ]
				ifFalse: [	Debugger openProcess: processToInterrupt context: processToInterrupt suspendedContext label: 'User Interrupt' ]
		] fork "Forked so the #userInterruptWatcher process continues."
	].! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/3/2022 14:23:22'!
newUIProcessIfCurrentCantRun
	"If the system needs a UIProcess (we know because UIProcess is not nil),
	then ensure that the UIProcess is ready to run, in order to hava a responsive UI.
	If we needed to create a new UI process, answer the old one, as it is most likely the
	process the user is interested in debugging. See senders."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			self spawnNewMorphicProcessFor: UI.
			^oldUIProcess ].
		UI millisecondsSinceLastCycle > 500 ifTrue: [
			self spawnNewMorphicProcessFor: UI.
			^oldUIProcess ].
		].
	^nil! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/3/2022 14:03:11'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	| doYield |
	doYield _ false.
	"If the old one gets resumed, don't step on each other feet"
	UIProcess ifNotNil: [
		UIProcess
			name: 'Formerly: ', UIProcess name;
			priority: UIProcess priority-1.
		UIProcess isRunning ifTrue: [ doYield _ true ]].
	UI _ guiRootObject.
	UIProcess _ guiRootObject runProcess.
	UIProcess resume.
	doYield ifTrue: [ Processor yield ]! !


!WorldMorph methodsFor: 'stepping' stamp: 'jmv 8/3/2022 14:17:00'!
runDeferredUIMessages
	"Processing the queue until empty is wrong if a block in it calls #addDeferredUIMessage: itself, because this loop will never end.
	Instead, process no more than entries already in queue befor we start iterating!!"
	| readyToProcess |
	readyToProcess _ deferredUIMessages size.
	readyToProcess timesRepeat: [
		UISupervisor isUIProcessRunning ifFalse: [ ^self ].
		deferredUIMessages nextOrNil ifNotNil: [ :block |
			block value ]].! !

