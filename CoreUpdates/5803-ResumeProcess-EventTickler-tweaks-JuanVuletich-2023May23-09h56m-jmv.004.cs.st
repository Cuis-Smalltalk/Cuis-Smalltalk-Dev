'From Cuis 6.0 [latest update: #5802] on 23 May 2023 at 12:44:02 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 5/23/2023 11:57:41'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.
	interruptedProcess isTerminated ifFalse: [
		(UISupervisor isUIProcessRunning and: [ UISupervisor ui == interruptedProcessUI]) ifTrue: [
			(interruptedProcess name withoutPrefix: 'Formerly: ')
				ifNotNil: [ :originalName | interruptedProcess name: originalName ].
			interruptedProcess priority: Processor userSchedulingPriority.
			UISupervisor uiProcess: interruptedProcess.
			mustTerminateActive := true ].
		interruptedProcess resume ].
	interruptedProcess := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorld ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !


!EventSensor methodsFor: 'private' stamp: 'jmv 5/23/2023 12:37:02'!
eventTickler
	"If the UI process hasn't processed events in a while, do it here.
	This is needed to detect the interrupt key."

	| delay |
	delay := Delay forMilliseconds: self class eventPollPeriod.
	[
		[
			| delta |
			"Could happen if process is halted and resumed."
			delay beingWaitedOn ifTrue: [ delay := Delay forMilliseconds: delay delayDuration ].
			delay wait.
			delta := Time localMillisecondClock - self lastEventPoll.
			"If the UI (i.e. Morphic) hasen't #nextEvent since the last time we checked,
			process events to allow for the interrupt key to be detected."
			delta >= self class eventPollPeriod ifTrue: [
				Cursor currentCursor = Cursor defaultCursor
					ifTrue: [ (Cursor cursorAt: #waitCursor) activateCursor ].
				"Discard any mouse events. This code is run when the UI is slow, essentially to have a working
				interrupt key. Processing mouse events is pointless: the UI will not handle them anyway.
				In addition, at least on Windows 7, when the machine is suspended and resumed with Cuis
				running, a lot of meaningless mouseMove events with the same coordinates are sent, maing
				Cuis extremely slow and CPU hungry for a few minutes without reason. Discarding mouse
				events makes the 'processing' of those very quick."
				self fetchMoreEventsDiscardingMouseEvents: true ]
		] on: UnhandledError 	do: [ :ex | 			ex return ]
	] repeat! !

!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 5/23/2023 12:33:51'!
fetchMoreEventsDiscardingMouseEvents: discardMouseEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore := true.
			inputSemaphore initSignals ].

	"DebuggingAid""
	dd _ Time localMillisecondClock - lastEventPoll.
	"

	eventBuffer := Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		"DebuggingAid""
		eventBuffer first = 2 ifTrue: [
			k1 _ eventBuffer third.
			u1 _ eventBuffer sixth.
			pressType _ eventBuffer fourth.
			typeS _ 'unknown'.
			pressType = EventSensor eventKeyDown ifTrue: [
				typeS _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				typeS _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				typeS _ #keystroke].
			dd >100 ifTrue: ['-' print. dd _ 0 ].
			{ k1 < 256 ifTrue: [Character numericValue: k1] ifFalse: [ '[]']. UnicodeCodePoint codePoint: k1. k1. k1 hex. '-----'. 
				UnicodeCodePoint codePoint: u1. u1. u1 hex. '----'. pressType. typeS } print.
			].
		"
		type := eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ].

	"Remember the last time that I checked for events."
	lastEventPoll := Time localMillisecondClock.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Sensor installEventTickler!

