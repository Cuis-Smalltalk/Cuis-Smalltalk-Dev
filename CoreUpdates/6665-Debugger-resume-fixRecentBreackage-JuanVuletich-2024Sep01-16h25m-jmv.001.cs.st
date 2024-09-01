'From Cuis7.1 [latest update: #6664] on 1 September 2024 at 4:26:08 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 9/1/2024 16:11:41'!
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
					UISupervisor uiProcess: interruptedProcess.
					UISupervisor ui mainCanvas unpinAndFreeMemory ]
				ifFalse: [
					"The Display has changed size while running the debugger.
					The interupted process would use an invalid canvas, corrupting the Display,
					and likely crashing the system. Don't resume the process. Just keep the current UI process running."
					interruptedProcess terminate.
					interruptedProcess := nil.
					interruptedProcessUI unpinAndFreeMemory.
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


!WorldMorph methodsFor: 'canvas' stamp: 'jmv 9/1/2024 16:14:59'!
mainCanvas: aMorphicCanvas
	"Set the main canvas"

	canvas == aMorphicCanvas ifFalse: [
		(Preferences at: #saveReleaseOptionsInWorldMenu) ifFalse: [
			aMorphicCanvas pinArrays ].
		canvas := aMorphicCanvas.
	].
	damageRecorder := DamageRecorder new.
	damageRecorder recordInvalidRect: self localBounds for: nil.
	waitDelay := Delay forMilliseconds: 50.! !

