'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 11:38:48 am'!

!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 11:10:23'!
openWindowLabel: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."

	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	UISupervisor whenUIinSafeState: [
		self openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil ].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfCurrentCantRun.
	] fork.! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 11:36:34'!
openContext: aContext label: label contents: contentsStringOrNil
	"Open a notifier (a PreDebugWindow) in response to an error, halt, or notify.
	A PreDebugWindow just shows a short view of the sender stack and provides a menu that lets the user open a full debugger.
	The context argument is in the active process stack."

	| debugger |
	<primitive: 19> "Simulation guard"

	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk logError: label inContext: aContext to: 'CuisDebug'].
	self informExistingDebugger: aContext label: label.

	debugger _ Debugger new.
	debugger 		process: Processor activeProcess context: aContext.
	debugger openWindowLabel: label usePreDebugWindow: true preDebugMessage: contentsStringOrNil.
	Processor activeProcess suspend.! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 11:33:32'!
openProcess: process context: context label: labelString
	"Open a Debugger on an existing process."

	| debugger logFile label |
	<primitive: 19> "Simulation guard"
	label _ labelString,
					' - Process: ', process name,
					' - Priority: ', process priority printString.
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		logFile _ 'CuisDebug'.
		(labelString beginsWith: 'Space is low')
			ifTrue: [logFile _ 'LowSpaceDebug'].
		Smalltalk logError: label inContext: context to: logFile ].

	UISupervisor newProcessIfUI: process.
	
	debugger _ Debugger new.
	debugger process: process context: context.
	debugger openWindowLabel: label usePreDebugWindow: false preDebugMessage: nil.
	process suspend.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 8/2/2022 17:35:58'!
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
		Debugger openProcess: p context: p suspendedContext label: 'Space is low' ]! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/2/2022 18:11:21'!
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
		] fork
	].! !

!methodRemoval: Debugger class #interruptProcess:label: stamp: 'jmv 8/3/2022 11:15:47'!
Debugger class removeSelector: #interruptProcess:label:!
!methodRemoval: Debugger class #interruptProcess:label:preDebugMessage: stamp: 'jmv 8/3/2022 11:27:03'!
Debugger class removeSelector: #interruptProcess:label:preDebugMessage:!
!methodRemoval: Debugger class #openInterrupt:onProcess: stamp: 'jmv 8/3/2022 11:15:53'!
Debugger class removeSelector: #openInterrupt:onProcess:!
!methodRemoval: Debugger class #openInterrupt:onProcess:preDebugMessage: stamp: 'jmv 8/3/2022 11:27:07'!
Debugger class removeSelector: #openInterrupt:onProcess:preDebugMessage:!
!methodRemoval: Debugger class #open2On:context:label:usePreDebugWindow: stamp: 'jmv 8/3/2022 11:31:21'!
Debugger class removeSelector: #open2On:context:label:usePreDebugWindow:!
!methodRemoval: Debugger #openNotifierContents:label: stamp: 'jmv 8/3/2022 11:17:53'!
Debugger removeSelector: #openNotifierContents:label:!
