'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 11:47:01 am'!

!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 11:45:44'!
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
		self openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil ].
	"This is in case the UI process is (for example) blocked before it processes deferred blocks."
	[
		(Delay forMilliseconds: 200) wait.
		UISupervisor newUIProcessIfCurrentCantRun.
	] fork.
	interruptedProcess suspend.! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 11:45:54'!
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
	debugger openWindowLabel: label usePreDebugWindow: true preDebugMessage: contentsStringOrNil.! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/3/2022 11:45:48'!
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
	"
[1/0] fork.
5 seconds asDelay wait.
self error: 'error'.
	"! !

