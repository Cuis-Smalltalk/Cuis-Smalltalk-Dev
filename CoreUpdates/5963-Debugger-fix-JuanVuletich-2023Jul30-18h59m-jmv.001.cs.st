'From Cuis 6.0 [latest update: #5962] on 30 July 2023 at 7:01:07 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 7/30/2023 19:00:39'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.
	interruptedProcess isTerminated ifFalse: [
		interruptedProcessUI notNil ifTrue: [
			(interruptedProcess name withoutPrefix: 'Formerly: ')
				ifNotNil: [ :originalName | interruptedProcess name: originalName ].
			interruptedProcess priority: Processor userSchedulingPriority.
			UISupervisor isUIProcessRunning
				ifTrue: [ mustTerminateActive := true]
				ifFalse: [ UISupervisor stopUIProcess].
			UISupervisor uiProcess: interruptedProcess ].
		interruptedProcess resume ].
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

