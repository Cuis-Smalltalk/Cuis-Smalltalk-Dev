'From Cuis 6.0 [latest update: #5437] on 5 August 2022 at 3:51:13 pm'!

!Debugger methodsFor: 'initialization' stamp: 'jmv 8/5/2022 15:51:00'!
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
		"Suspend it as early as possible"
		interruptedProcess suspend.
		self openWindow2Label: label usePreDebugWindow: bool preDebugMessage: preDebugMessageOrNil ].
	"Ensure the Debugger is opened in a new UI process.
	This avoid unresponsive UI if the interruptedProcess is the UI current UI process, but also if the UI is busy, blocked or suspended."
	UISupervisor newUIProcess.
	"Suspend it as early as possible"
	interruptedProcess suspend.! !

