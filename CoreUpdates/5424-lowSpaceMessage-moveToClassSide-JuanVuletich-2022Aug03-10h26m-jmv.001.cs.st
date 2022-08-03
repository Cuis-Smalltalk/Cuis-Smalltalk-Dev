'From Cuis 6.0 [latest update: #5421] on 3 August 2022 at 10:31:09 am'!

!Debugger class methodsFor: 'constants' stamp: 'jmv 8/2/2022 16:46:24'!
lowSpaceMessage
	"Return a notifier message string to be presented when space is running low."

	^ 'Warning!! Cuis is almost out of memory!!

Low space detection is now disabled. It will be restored when you close or proceed from this error notifier. Don''t panic, but do proceed with caution.

Here are some suggestions:

- If you suspect an infinite recursion (the same methods calling each other again and again), then close this debugger, and fix the problem.

- If you want this computation to finish, then make more space available (read on) and choose "proceed" in this debugger. Here are some ways to make more space available...
   > Close any windows that are not needed.
   > Get rid of some large objects (e.g., images).
   > Leave this window on the screen, choose "save as..." from the screen menu, quit, restart the Squeak VM with a larger memory allocation, then restart the image you just saved, and choose "proceed" in this window.

- If you want to investigate further, choose "debug" in this window.  Do not use the debugger "fullStack" command unless you are certain that the stack is not very deep. (Trying to show the full stack will definitely use up all remaining memory if the low-space problem is caused by an infinite recursion!!).

'
! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 8/3/2022 10:30:44'!
openNotifierContents: msgString label: label
	"Create and schedule a notifier view with the given label and message. A notifier view shows just the message or the first several lines of the stack, with a menu that allows the user to open a full debugger if so desired."
	"NOTE: When this method returns, a new process has been scheduled to run the windows, and thus this notifier, but the previous active porcess has not been suspended.  The sender will do this."
	| msg |
	Sensor flushKeyboard.
	savedCursor _ Cursor currentCursor.
	Cursor defaultCursor activateCursor.
	msg _ (label beginsWith: 'Space is low')
		ifTrue: [ self class lowSpaceMessage, (msgString ifNil: [ '' ]) ]
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

!methodRemoval: Debugger #lowSpaceChoices stamp: 'jmv 8/3/2022 10:30:48'!
Debugger removeSelector: #lowSpaceChoices!
