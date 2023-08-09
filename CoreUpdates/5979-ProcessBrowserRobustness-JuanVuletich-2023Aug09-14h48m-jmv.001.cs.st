'From Cuis 6.0 [latest update: #5978] on 9 August 2023 at 2:53:58 pm'!

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 8/9/2023 14:51:55'!
findPigProcess
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process |
	promise := Processor tallyCPUUsageFor: 0.1 every: 10.
	tally := promise value.
	tally ifEmpty: [ ^nil ]. "We can not do better"
	"Avoid, system processes, the background process, and terminated processes."
	tally sortedCounts do: [ :entry |
		process := entry value.
		(process == Processor backgroundProcess or: [ (process name beginsWith: '[system]') or: [process isTerminated]])
			ifFalse: [ ^process ]].
	^nil! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/9/2023 14:51:42'!
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
	"Shift can only be detected on the Mac, due to likely buggy kestroke reporting..."
	processToInterrupt := Sensor shiftPressed | (Preferences at: #cmdDotInterruptTakesStatistics)
		ifTrue: [Utilities reportCPUandRAM]
		ifFalse: [Utilities processTakingMostCPU].
	oldUIProcess := UISupervisor newUIProcessIfCurrentCantRun.
	processToInterrupt isNil ifTrue: [ processToInterrupt := oldUIProcess ].
	processToInterrupt isNil ifTrue: [ processToInterrupt := Processor preemptedProcess ].
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

