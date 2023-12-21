'From Cuis6.0 [latest update: #6147] on 21 December 2023 at 4:44:12 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 12/21/2023 16:38:16'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.
	interruptedProcess isTerminated ifFalse: [
		interruptedProcessUI notNil ifTrue: [
			(interruptedProcess name withoutPrefix: 'Formerly: ') ifNotNil: [ :originalName |
				interruptedProcess
					name: originalName;
					priority: interruptedProcess priority + 1 ].
			UISupervisor isUIProcessRunning
				ifTrue: [ mustTerminateActive := true]
				ifFalse: [ UISupervisor stopUIProcess].
			UISupervisor uiProcess: interruptedProcess ].
		interruptedProcess resolvePendingQuickReturns; resume ].
	interruptedProcess := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	mustRedisplay ifNotNil: [ :w |
		[UISupervisor whenUIinSafeState: [ w invalidateBounds ]]
			forkAt: Processor userSchedulingPriority-1 ].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !

