'From Cuis7.1 [latest update: #6665] on 2 September 2024 at 2:35:27 pm'!

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 9/2/2024 14:25:19'!
setupCanvas
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."

	| newCanvas |
	newCanvas := [ MorphicCanvas onDisplayToReplace: canvas ]
				on: OutOfMemory
				do: [
					"In case of lack of memory, use a much cheaper (but limited) BitBltCanvas."
					'Not enough memory to run VectorEngine. Using BitBltCanvas instead.' print.
					MorphicCanvas activeSubclass: BitBltCanvas.
					MorphicCanvas onDisplay ].
	self mainCanvas: newCanvas.
	canvas world: self.! !


!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 9/2/2024 11:26:20'!
vectorEngine
	"Answer our VectorEngineDrawer, or nil if we don't have one."
	^nil! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 9/2/2024 12:01:47'!
doOwnEngine
	"VectorCanvas needs this."! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 9/2/2024 14:25:08'!
onDisplay
	"Answer an instance of the appropriate subclass targetting Display,
	or an intermediate buffer, as appropriate.
	Create a new VectorEngineDrawer or BitBltCanvasEngine as appropriate."
	        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"

	| canvasTarget |
	canvasTarget := (Display deferUpdates: true)
		"Both Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display."
		ifNotNil: [ Display ]
		"Mac (at least on Intel) doesn't support deferUpdates:, so Morphic Canvas target is this aux Form."
		ifNil: [ Form extent: Display extent depth: Display depth ].
	canvasTarget == Display ifTrue: [
		"But then, they need to flush updates periodically."
		DisplayScreen installScreenUpdater ].
	^self onForm: canvasTarget.! !

!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 9/2/2024 14:25:34'!
onDisplayToReplace: oldCanvasOrNil
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."

	| vectorEngineToReuse |
	self subclassToUse = oldCanvasOrNil class ifFalse: [
		^self onDisplay ].
	vectorEngineToReuse := oldCanvasOrNil vectorEngine.
	vectorEngineToReuse isNil ifTrue: [
		^self onDisplay ].
	vectorEngineToReuse isConsistentWithTarget ifFalse: [
		^self onDisplay ].
	^self subclassToUse basicNew
		setVectorEngine: vectorEngineToReuse;
		initializeWithTranslation: `0@0`.! !


!VectorCanvas methodsFor: 'accessing' stamp: 'jmv 9/2/2024 11:26:31'!
vectorEngine
	"Answer our VectorEngineDrawer, or nil if we don't have one."
	^engine! !

!VectorCanvas methodsFor: 'initialization' stamp: 'jmv 9/2/2024 12:02:38'!
setVectorEngine: vectorEngineToReuse

	form := vectorEngineToReuse target.
	engine := vectorEngineToReuse.
	self doOwnEngine.
	auxBitBltEngine := BitBltCanvasEngine toForm: form.
	self newClipRect: nil.! !

!VectorCanvas methodsFor: 'private' stamp: 'jmv 9/2/2024 12:02:22'!
doOwnEngine
	"VectorCanvas needs this."
	engine canvas: self.! !


!HybridCanvas methodsFor: 'accessing' stamp: 'jmv 9/2/2024 11:26:47'!
vectorEngine
	"Answer our VectorEngineDrawer, or nil if we don't have one."
	^vectorCanvas vectorEngine! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 9/2/2024 12:02:03'!
doOwnEngine
	"VectorCanvas needs this."
	vectorCanvas doOwnEngine! !

!HybridCanvas methodsFor: 'private' stamp: 'jmv 9/2/2024 11:57:57'!
setVectorEngine: vectorEngineToReuse

	form := vectorEngineToReuse target.
	engine := BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas := VectorCanvas basicNew
		setVectorEngine: vectorEngineToReuse.
	vectorCanvas attachedCanvas: self.
	morphIDsForm := Form extent: form extent depth: 32 bits: vectorCanvas engine morphIds.
	drawingOnMorphIDs := false.
	self newClipRect: nil.! !


!VectorEngineDrawer methodsFor: 'accessing' stamp: 'jmv 9/2/2024 11:42:35'!
target
	^targetForm! !


!Debugger methodsFor: 'private' stamp: 'jmv 9/2/2024 14:18:58'!
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
					The interupted process would use an invalid canvas, corrupting the Display, likely
					crashing the system. Don't resume the process. Just keep the current UI process running."
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


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 9/2/2024 14:20:33'!
mainLoop

	self position: `0@0` extent: Display extent.
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."
	self setupCanvas.
	self restoreDisplay.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

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

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/2/2024 14:23:58'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e p needsNewUIProcessAndCanvas |

	"Linux VMs continuously update #actualScreenSize while the user is resizing the OS window.
	Avoid reallocation of Display until the user is done with that.
	Currently, wait until a real event happens inside Cuis.
	This requires the user to move the mouse back into Cuis.
	Maybe other ways are possible."
	lastCycleHadAnyEvent ifFalse: [
		Smalltalk osVersion = 'linux-gnu' ifTrue: [ 		^self ]].

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
			"Create a new UI process, including a new canvas consistent with updated Display.
			This will also terminate the current process, and this is needed because the current canvas can no longer be used.
			A canvas + vector engine inconsistent with Display will cause invalid memory accesses, crashing the system."
			UISupervisor spawnNewMorphicProcessFor: self andTerminateCurrent: true ].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 9/2/2024 12:02:49'!
beMainCanvasOfWorld
	world mainCanvas: self.
	self doOwnEngine.! !


!HybridCanvas methodsFor: 'private' stamp: 'jmv 9/2/2024 11:57:47'!
setForm: aForm subPixelAntiAliasing: aBooleanOrNil
	"nil means use default kind of anti aliasing"

	form := aForm.
	engine := BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	vectorCanvas := VectorCanvas onForm: form.
	vectorCanvas attachedCanvas: self.
	morphIDsForm := Form extent: form extent depth: 32 bits: vectorCanvas engine morphIds.
	drawingOnMorphIDs := false.
	self newClipRect: nil.! !


!VectorEngineDrawer methodsFor: 'private' stamp: 'jmv 9/2/2024 14:21:24'!
pvtSetForm: aForm

	| s |
	targetForm := aForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	s := targetWidth * targetHeight.
	self flag: #VEPapi5.		"Eventually remove the +1.
	Needed for VEPapi=5 and 6 (for plugins older than Sept-21-2021, and version less than 1.29).
	Avoid it altoghether if the plugin is not available (#pluginApiVersion answers 0)"
	morphIds := WordArray new: (
		(VectorEngineWithPlugin pluginApiVersion between: 5 and: 6)
			ifTrue: [s+1]
			ifFalse: [s]).
	edgeCounts := self auxPixelDataClass new: s.
	alphaMask := self auxPixelDataClass new: s.
	contour := Float32Array new: targetHeight * 2.
	0 to: targetHeight-1 do: [ :y |
		contour at: y * 2 + 1 put: targetWidth.
		contour at: y * 2 + 2 put: 0 ].
	self resetMorphBounds.
	self clipRect: targetForm boundingBox.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	geometryTransformation := MorphicTranslation identity.! !

!VectorEngineDrawer methodsFor: 'services' stamp: 'jmv 9/2/2024 12:53:43'!
isConsistentWithTarget
	"and display!!!!!!!!!!"
	targetForm isNil ifTrue: [ ^false ].
	targetForm extent = Display extent ifFalse: [ ^false ].
	^edgeCounts size = (targetWidth * targetHeight) and: [
		targetWidth = targetForm width and: [ targetForm height = targetHeight ]].! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 9/2/2024 14:27:50'!
newUIProcess
	"Start a new UI process for the current UI.
	The current UI process (if any) will exit at the end of the current UI cycle."
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."

	self spawnNewMorphicProcessFor: UI.! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 9/2/2024 14:29:41'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."
	self spawnNewMorphicProcessFor: guiRootObject andTerminateCurrent: false.! !

!methodRemoval: Form #getMainCanvas stamp: 'jmv 9/2/2024 14:21:34'!
Form removeSelector: #getMainCanvas!
