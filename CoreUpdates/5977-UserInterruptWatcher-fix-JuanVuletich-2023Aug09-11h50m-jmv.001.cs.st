'From Cuis 6.0 [latest update: #5976] on 9 August 2023 at 11:51:13 am'!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/9/2023 11:22:43'!
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

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Sensor installInterruptWatcher!

