'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 11:55:12 am'!

!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 11:51:50'!
openWindowLabel: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil
	"Create and schedule a full debugger, or just a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."

	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	"In case an error in Morphic code got us here, ensure mouse focus has been released"
	true runningWorld ifNotNil: [ :rWorld |
		rWorld activeHand ifNotNil: [ :aHand |
			aHand releaseMouseFocus ]].

	UISupervisor whenUIinSafeState: [
		interruptedProcess suspend.
		self openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil ].
	"Ensure the Debugger is opened in a new UI process.
	This avoid unresponsive UI if the interruptedProcess is the UI current UI process, but also if the UI is busy, blocked or suspended."
	UISupervisor newUIProcess.! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 11:53:51'!
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

	debugger _ Debugger new.
	debugger process: process context: context.
	debugger openWindowLabel: label usePreDebugWindow: false preDebugMessage: nil.
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !

!methodRemoval: UISupervisor class #newProcessIfUI: stamp: 'jmv 8/3/2022 11:54:06'!
UISupervisor class removeSelector: #newProcessIfUI:!
