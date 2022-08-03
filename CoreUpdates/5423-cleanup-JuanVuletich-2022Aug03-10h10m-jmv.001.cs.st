'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 10:26:18 am'!

!Process methodsFor: 'debugging' stamp: 'jmv 8/3/2022 10:24:48'!
debugContext: context title: title
	"Open debugger on self with context shown on top"

	| topCtxt |
	self isTerminated ifTrue: [^ self error: 'can not debug a terminated process'].
	topCtxt _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	(topCtxt hasContext: context) ifFalse: [^ self error: 'context not in process'].
	Debugger openOn: self context: context label: title fullView: true.
! !

!Process methodsFor: 'debugging' stamp: 'jmv 8/3/2022 10:25:21'!
debugWithTitle: title
	"Open debugger on self"

	| context |
	context _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	self debugContext: context title: title! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/2/2022 18:04:26'!
newUIProcess
	"Start a new UI process for the current UI.
	The current UI process (if any) will exit at the end of the current UI cycle."

	self spawnNewMorphicProcessFor: UI.! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 8/2/2022 18:11:19'!
newUIProcessIfCurrentCantRun
	"If the system needs a UIProcess (we know because UIProcess is not nil),
	then ensure that the UIProcess is ready to run, in order to hava a responsive UI.
	If we needed to create a new UI process, answer the old one, as it is most likely the
	process the user is interested in debugging. See senders."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			self spawnNewMorphicProcessFor: UI.
			^oldUIProcess ]].
	^nil! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 10:13:01'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self lowSpaceChoices , (msgString ifNil: [ '' ]) ]
		ifFalse: [ msgString ].
	interruptedProcessUI _ UISupervisor newProcessIfUI: interruptedProcess.
	UISupervisor whenUIinSafeState: [
		PreDebugWindow
			forceOpen: self
			label: label
			message: msg ].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfCurrentCantRun.
	] fork.! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 10:13:06'!
openOn: process context: context label: title fullView: bool
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."
	| guiRootObjectOrNil |
	(Preferences at: #logDebuggerStackToFile) ifTrue: [
		Smalltalk
			logError: title
			inContext: context
			to: 'CuisDebug' ].
	guiRootObjectOrNil _ UISupervisor newProcessIfUI: process.
	"schedule debugger in deferred UI message to address redraw
	problems after opening a debugger e.g. from the testrunner."
	UISupervisor whenUIinSafeState: [
		self openOn: process context: context label: title fullView: bool guiRootObject: guiRootObjectOrNil
		].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfCurrentCantRun.
	] fork.
	process suspend.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 8/3/2022 10:22:01'!
debugProcess: aProcess
"	self resumeProcess: aProcess."
	aProcess debugWithTitle: 'Interrupted from the Process Browser'.
! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 8/3/2022 10:13:10'!
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
				ifFalse: [ Debugger interruptProcess: processToInterrupt label: 'User Interrupt' ]] fork
		].! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 8/2/2022 17:13:59'!
devDefaultAction

	Processor activeProcess
		debugContext: exception signalerContext
		title: exception description.! !


!StackSizeWatcher methodsFor: 'private' stamp: 'jmv 8/3/2022 10:22:05'!
debug: aProcess

	aProcess debugWithTitle: 'Interrupted - Stack too deep'.
! !

!methodRemoval: UISupervisor class #newUIProcessIfNeeded stamp: 'jmv 8/3/2022 10:13:18'!
UISupervisor class removeSelector: #newUIProcessIfNeeded!
!methodRemoval: Process #debug:title: stamp: 'jmv 8/3/2022 10:18:51'!
Process removeSelector: #debug:title:!
!methodRemoval: Process #debugFullWithTitle: stamp: 'jmv 8/3/2022 10:22:25'!
Process removeSelector: #debugFullWithTitle:!
!methodRemoval: Process #debug:title:full: stamp: 'jmv 8/3/2022 10:25:26'!
Process removeSelector: #debug:title:full:!
