'From Cuis7.1 [latest update: #6656] on 29 August 2024 at 5:34:26 pm'!

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 8/29/2024 11:54:41'!
mainCanvas: aMorphicCanvas
	"Set the main canvas"

	canvas := aMorphicCanvas.! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 8/29/2024 11:56:18'!
beMainCanvasOfWorld
	world mainCanvas: self! !


!Debugger methodsFor: 'initialization' stamp: 'jmv 8/29/2024 11:46:05'!
windowIsClosing
	"My window is being closed; clean up. Restart the low space watcher."

	interruptedProcess
		ifNil: [^ self].
	interruptedProcess terminate.
	interruptedProcess := nil.
	interruptedProcessUI := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	Smalltalk installLowSpaceWatcher.  "restart low space handler"
! !

!Debugger methodsFor: 'private' stamp: 'jmv 8/29/2024 11:56:23'!
resumeProcess
	| mustTerminateActive worldIfMustRedisplay |
	worldIfMustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.
	interruptedProcess isTerminated ifFalse: [
		interruptedProcessUI notNil ifTrue: [
			interruptedProcessUI isConsistentWithTarget
				ifTrue: [
					(interruptedProcess name withoutPrefix: 'Formerly: ') ifNotNil: [ :originalName |
						interruptedProcess
							name: originalName;
							priority: interruptedProcess priority + 1 ].
					UISupervisor isUIProcessRunning
						ifTrue: [ mustTerminateActive := true]
						ifFalse: [ UISupervisor stopUIProcess].
					UISupervisor uiProcess: interruptedProcess ]
				ifFalse: [
					"The Display has changed size while running the debugger.
					The interupted process would use an invalid canvas, corrupting the Display,
					and likely crashing the system. Don't resume the process. Just keep the current UI process running."
					interruptedProcess terminate.
					interruptedProcess := nil.
					interruptedProcessUI := nil ]].
		interruptedProcess ifNotNil: [ :p |
			interruptedProcessUI ifNotNil: [ :canvas |
				canvas beMainCanvasOfWorld ].
			p resolvePendingQuickReturns; resume ]].
	interruptedProcess := nil.
	interruptedProcessUI := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	worldIfMustRedisplay ifNotNil: [ :w |
		[UISupervisor whenUIinSafeState: [ w invalidateBounds ]]
			forkAt: Processor userSchedulingPriority-1 ].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ].! !

