'From Cuis6.0 [latest update: #6150] on 21 December 2023 at 5:00:15 pm'!

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/21/2023 16:37:03'!
uiProcessIfCantRun
	"If the system needs a UIProcess (we know because UIProcess is not nil),
	then ensure that the UIProcess is ready to run, in order to hava a responsive UI.
	If we needed to create a new UI process, answer the old one, as it is most likely the
	process the user is interested in debugging. See senders."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			^oldUIProcess ].
		UI millisecondsSinceLastCycle > 500 ifTrue: [
			^oldUIProcess ].
		].
	^nil! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/21/2023 16:58:10'!
handleUserInterrupt
	| processToInterrupt oldUIProcess |
	"If the UI process is not ready to run, the UI was not responsive, and most likely that's why the user pressed
	the interrupt key. Make GUI responsive again, and let them debug the process that made UI not responsive.
	'The four Andreas tests for interruptability':
	http://lists.squeakfoundation.org/pipermail/squeak-dev/2022-July/221727.html
		[true] whileTrue
		[[true] whileTrue] forkAt: Processor userSchedulingPriority + 1
		Smalltalk createStackOverflow
		[Smalltalk createStackOverflow] forkAt: Processor userSchedulingPriority + 1
	"
	oldUIProcess := UISupervisor uiProcessIfCantRun.
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	processToInterrupt := Sensor shiftPressed | (Preferences at: #cmdDotInterruptTakesStatistics)
		ifTrue: [Utilities reportCPUandRAM]
		ifFalse: [Utilities processTakingMostCPU].
	(Preferences at: #cmdDotEnabled) ifFalse: [
		^self ].
	(processToInterrupt isNil or: [ (self canBeInterrupted: processToInterrupt) notNil]) ifTrue: [
		processToInterrupt := oldUIProcess ifNil: [ Processor preemptedProcess ]].
	processToInterrupt isNil ifTrue: [
		'No process to interrupt found' print.
		^self ].
	(self canBeInterrupted: processToInterrupt) ifNotNil: [ :msg |
		msg print.
		^self ].
	[
		EventSensor install.
		Debugger openProcess: processToInterrupt context: processToInterrupt suspendedContext label: 'User Interrupt'
	] fork "Forked so the #userInterruptWatcher process continues."! !

!methodRemoval: UISupervisor class #newUIProcessIfCurrentCantRun stamp: 'jmv 12/21/2023 16:58:13'!
UISupervisor class removeSelector: #newUIProcessIfCurrentCantRun!
