'From Cuis7.1 [latest update: #6598] on 17 August 2024 at 6:18:46 pm'!

!MorphicCanvas methodsFor: 'other' stamp: 'jmv 8/16/2024 17:59:55'!
isConsistentWithTarget
	^true! !


!VectorCanvas methodsFor: 'other' stamp: 'jmv 8/16/2024 18:00:07'!
isConsistentWithTarget
	^engine isConsistentWithTarget! !


!HybridCanvas methodsFor: 'other' stamp: 'jmv 8/16/2024 18:15:03'!
isConsistentWithTarget
	^vectorCanvas isConsistentWithTarget! !


!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 8/17/2024 18:06:31'!
isConsistentWithTarget
	"and display!!!!!!!!!!"
	targetForm extent = Display extent ifFalse: [ ^false ].
	^edgeCounts size = (targetWidth * targetHeight) and: [
		targetWidth = targetForm width and: [ targetForm height = targetHeight ]].! !


!Debugger methodsFor: 'private' stamp: 'jmv 8/17/2024 18:08:06'!
process: aProcess context: aContext

	interruptedProcess := aProcess.
	interruptedProcessUI := (UISupervisor animatedUIOf: interruptedProcess) ifNotNil: [ :world | world mainCanvas ].
	contextStackTop := aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex := 1.! !

!Debugger methodsFor: 'private' stamp: 'jmv 8/17/2024 18:17:11'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay := self runningWorld.
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
					interruptedProcess := nil ]].
		interruptedProcess ifNotNil: [ :p | p resolvePendingQuickReturns; resume ]].
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
		ifTrue: [ Processor terminateActive ].! !

