'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 10:38:55 am'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 10:34:37'!
interruptProcess: aProcess label: labelString
	"Create a Notifier on aProcess with the given label.
	This method is called only in special system processes, like the #userInterruptWatcher and the #lowSpaceWatcher."
	|  label |

	label _ labelString,
					' - Process: ', aProcess name,
					' - Priority: ', aProcess priority printString.

	aProcess suspend.
	self
		openInterrupt: label 
		onProcess: aProcess! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 8/3/2022 10:34:10'!
lowSpaceWatcher
	"Wait until the low space semaphore is signalled, then take appropriate actions."

	| p |
	self garbageCollectMost <= self lowSpaceThreshold ifTrue: [
		self garbageCollect <= self lowSpaceThreshold ifTrue: [
			"free space must be above threshold before starting low space watcher"
			^ Smalltalk primitiveBeep ]].

	LowSpaceSemaphore _ Semaphore new.
	self primLowSpaceSemaphore: LowSpaceSemaphore.
	self primSignalAtBytesLeft: self lowSpaceThreshold.  "enable low space interrupts"

	LowSpaceSemaphore wait.  "wait for a low space condition..."

	self primSignalAtBytesLeft: 0.  "disable low space interrupts"
	self primLowSpaceSemaphore: nil.
	LowSpaceProcess _ nil.
	"Note: user now unprotected until the low space watcher is re-installed"

	p _ Processor preemptedProcess.
	p isTerminated ifFalse: [
		p suspend.
		Debugger interruptProcess: p label: 'Space is low' ]! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/3/2022 10:32:53'!
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
				ifFalse: [ Debugger interruptProcess: processToInterrupt label: 'User Interrupt' ]
		] fork
	].! !

