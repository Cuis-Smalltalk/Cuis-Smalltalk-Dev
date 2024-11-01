'From Cuis7.1 [latest update: #6802] on 29 October 2024 at 3:31:51 pm'!

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/28/2024 15:19:24'!
runProcess: createNewCanvas
	
	^[
		createNewCanvas ifTrue: [
			self position: `0@0` extent: Display extent.
			"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."
			self setupCanvas.
			self restoreDisplay ].
		"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
		Might happen, especially on RasPi or other systems without an RTC"
		lastCycleTime := Time localMillisecondClock.
		lastAlarmTime := 0.
		self mainLoop
	] newProcess.! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 10/28/2024 16:04:04'!
spawnNewMorphicProcessFor: guiRootObject createNewCanvas: createNewCanvas
	"
	aBoolean ifTrue terminate current process. Use with care.
	Maybe prefer the safer #spawnNewMorphicProcessFor:
	"
	| doYield |
	doYield := false.
	"If the old one gets resumed, don't step on each other feet"
	UIProcess ifNotNil: [
		UIProcess
			name: 'Formerly: ', UIProcess name;
			priority: UIProcess priority-1.
		UIProcess isRunning ifTrue: [ doYield := true ]].
	UI := guiRootObject.
	UISupervisor uiProcess: (guiRootObject runProcess: createNewCanvas).
	doYield ifTrue: [ Processor yield ].! !


!Debugger methodsFor: 'private' stamp: 'jmv 10/28/2024 15:57:52'!
resumeProcess
	| mustTerminateActive worldIfMustRedisplay |
	worldIfMustRedisplay := self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive := false.

	interruptedProcess isTerminated ifFalse: [
		interruptedProcess resolvePendingQuickReturns.
		interruptedProcessUI isNil
			ifTrue: [
				interruptedProcess resume ]
			ifFalse: [
				interruptedProcessUI isConsistentWithTarget
					ifTrue: [
						interruptedProcessUI beMainCanvasOfWorld.
						UISupervisor isUIProcessRunning
							ifTrue: [ mustTerminateActive := true]
							ifFalse: [ UISupervisor stopUIProcess].
						UISupervisor uiProcess: interruptedProcess ]
					ifFalse: [
						"The Display has changed size while running the debugger.
						The interupted process would use an invalid canvas, corrupting the Display, likely
						crashing the system. Don't resume the process. Just keep the current UI process running."
						interruptedProcess terminate ]]].

	"Cleanup"
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


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/28/2024 14:37:13'!
mainLoop

	[
		self displayWorldOn: canvas.
		[
			"Here canvas and waitDelay are passed as arguments.
			This is because when debugging Morphic, a new UI process,
			these ivars are updated, but in the debugger process we still want the original ones."
			self doOneCycleOn: canvas delay: waitDelay.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				context  selector = #drawOn: ])
			ifNotNil: [ :context |
				context receiver drawingFails.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger.
				This is to use a new, clean Canvas."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/28/2024 15:05:54'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p needsNewUIProcessAndCanvas |

	"Linux VMs continuously update #actualScreenSize while the user is resizing the OS window.
	Try to avoid reallocation of Display until the user is done with that.
	For that, wait until a real event happens inside Cuis. (This requires the user to move the mouse back into Cuis).
	Or use a timer, and do it no more often than every quarter a second, and only if the resize finishes/pauses for that time."
	DisplayScreen isDisplayExtentOk ifFalse: [
		Smalltalk osVersion = 'linux-gnu' ifTrue: [
			self valueOfProperty: #startForNewDisplayTimeout ifAbsentPut: [Time localMillisecondClock].
			self valueOfProperty: #newDisplayOldExtent ifAbsentPut: [DisplayScreen actualScreenSize].
			lastCycleHadAnyEvent ifFalse: [
				(self valueOfProperty: #newDisplayOldExtent) = DisplayScreen actualScreenSize ifFalse: [
					"If user still resizing, restart timer: wait until the user finishes resizing."
					self setProperty: #startForNewDisplayTimeout toValue: Time localMillisecondClock.
					self setProperty: #newDisplayOldExtent toValue: DisplayScreen actualScreenSize].
				Time localMillisecondClock - (self valueOfProperty: #startForNewDisplayTimeout) < 250 ifTrue: [
					^self ]].
			self removeProperty: #startForNewDisplayTimeout.
			self removeProperty: #newDisplayOldExtent ]].

	needsNewUIProcessAndCanvas := false.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"''print. {DateAndTime now. 'Smalltalk Display:'. Display extent. 'os display:'. DisplayScreen actualScreenSize} print."
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := DisplayScreen hostOSWindowExtent ifNotNil: [ :ee |
				(ee * ScaleFactor - DisplayScreen actualScreenSize + Display extent) truncated ].
			p := DisplayScreen hostOSWindowPosition.
			e ifNotNil: [
				"{'New ScaleFactor'. ScaleFactor. 'Set os display to:'. e} print."
				DisplayScreen hostOSWindowExtent: e].
			p ifNotNil: [DisplayScreen hostOSWindowPosition: p].
		].
		"When moving the Cuis window, for instance, between a laptop screen with ScaleFactor=2.0 and an external screen with
		ScaleFactor=1.0, usually the #hostOSWindowExtent: already left us with isDisplayExtentOk, so asking again is in order."
		DisplayScreen isDisplayExtentOk ifFalse: [
			"This allocates new memory for Display and is critical."
			needsNewUIProcessAndCanvas := true.
			"This may help with memory fragmentation issues possibly making new display allocation fail."
			canvas unpinAndFreeMemory.
			DisplayScreen setupDisplay ]].
	(needsNewUIProcessAndCanvas or: [canvas isConsistentWithTarget not])
		ifTrue: [
			self whenUIinSafeState: [ Cursor defaultCursor activateCursor ].
			"The current canvas is not consistent with Display, and therefore can no longer be used. A new one is required.
			Freeeing the memory it uses may help with memory fragmentation issues possibly making new canvas allocation fail.
			(If there is not enough free memory for VectorCanvas and engine, use a BitBltCanvas, see #getMainCanvas)"
			canvas unpinAndFreeMemory.
			"Create a new UI process, including a new canvas consistent with updated Display."
			UISupervisor spawnNewMorphicProcessFor: self createNewCanvas: true.
			"Terminate the current process (the old UI process). This is needed because the old canvas can no longer be used:
			A canvas + vector engine inconsistent with Display will cause invalid memory accesses, crashing the system."
			Processor terminateActive.
			].! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 10/28/2024 15:00:50'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."
	self spawnNewMorphicProcessFor: guiRootObject createNewCanvas: true.! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 10/28/2024 15:57:01'!
uiProcess: aProcess
	UIProcess := aProcess.
	UIProcess
		name: 'Morphic UI';
		priority: Processor userSchedulingPriority;
		resume.! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 10/28/2024 15:23:23'!
uiProcessIfCantRun
	"If the system needs a UIProcess (we know because UIProcess is not nil),
	then ensure that the UIProcess is ready to run, in order to hava a responsive UI.
	If we needed to create a new UI process, answer the old one, as it is most likely the
	process the user is interested in debugging.
	If there is an UIProcess, and it can run, answer nil.
	If there's no UIProcess at all, also answer nil.
	See senders."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			^oldUIProcess ].
		UI millisecondsSinceLastCycle > 500 ifTrue: [
			^oldUIProcess ].
		].
	^nil! !

!methodRemoval: UISupervisor class #spawnNewMorphicProcessFor:andTerminateCurrent: stamp: 'jmv 10/29/2024 15:31:25'!
UISupervisor class removeSelector: #spawnNewMorphicProcessFor:andTerminateCurrent:!
!methodRemoval: WorldMorph #runProcess stamp: 'jmv 10/29/2024 15:31:25'!
WorldMorph removeSelector: #runProcess!
