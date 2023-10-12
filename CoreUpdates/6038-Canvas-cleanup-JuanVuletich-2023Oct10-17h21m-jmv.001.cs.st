'From Cuis 6.0 [latest update: #6038] on 10 October 2023 at 5:22:12 pm'!

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/10/2023 15:58:35'!
displayWorld
	"Update this world's display."

	| allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"Restore world canvas under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	canvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage := canvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage := (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage := h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	canvas drawsOnDisplay ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 10/10/2023 16:18:12'!
mainLoop

	| morphicCanvas d |
        "If this platform supports deferred updates
		- Set Display to deferred update mode
		- Use a Canvas on Display
	otherwise
		- Set a non-Display canvas"
	self morphPosition: `0@0` extent: Display extent.
	morphicCanvas := (Display deferUpdates: true)
		ifNotNil: [ Display getMainCanvas ]
		ifNil: [ MorphicCanvas onForm: (Form extent: extent depth: Display depth) ].
	morphicCanvas world: self.
	damageRecorder := DamageRecorder new.
	self restoreDisplay.
	canvas := morphicCanvas.

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime := Time localMillisecondClock.
	lastAlarmTime := 0.

	d := Delay forMilliseconds: 50.
	[
		self displayWorld.
		[
			self doOneCycle: d.
			UISupervisor isUIProcessRunning ]
				whileTrue: []
	] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		(ex signalerContext findContextSuchThat: [ :context |
				(context receiver is: #Morph) and: [ context  selector = #drawOn: ]])
			ifNotNil: [ :context |
				context receiver drawingFails.
				self clearCanvas.
				"Install the old error handler, so we can re-raise the error.
				This means, open a Debugger.
				This will also create a new Morphic process. if the user closes the debugger."
				ex receiver error: ex description.
				"Create a new UI process, if the user proceeds in the debugger."
				UISupervisor newUIProcess ]
			ifNil: [ ex pass ]
	].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/10/2023 16:15:49'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	| newScaleFactor e d p |
	DisplayScreen isDisplayExtentOk ifFalse: [
		"Minimize the risk of going out of memory:
		- First clear existing canvas, to free the memory it uses.
		- Then, setup the display.
		- Then set up new canvas."
		self clearCanvas.
		"If host OS scale factor has changed, try to recover our previous Display extent"
		newScaleFactor := DisplayScreen hostOSScaleFactor.
		newScaleFactor = ScaleFactor ifFalse: [
			ScaleFactor := newScaleFactor.
			e := Display extent.
			d := DisplayScreen hostOSWindowExtent * ScaleFactor - DisplayScreen actualScreenSize.
			p := DisplayScreen hostOSWindowPosition.
			DisplayScreen hostOSWindowExtent: e+d.
			DisplayScreen hostOSWindowPosition: p.
		].
		DisplayScreen setupDisplay.
		UISupervisor newUIProcess.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 10/10/2023 16:16:15'!
activeSubclass: aMorphicCanvasSubclass
	"
	self runningWorld canvas class
	MorphicCanvas activeSubclass: BitBltCanvas
	MorphicCanvas activeSubclass: HybridCanvas
	MorphicCanvas activeSubclass: VectorCanvas
	"
	(#(BitBltCanvas HybridCanvas VectorCanvas)
		includes: aMorphicCanvasSubclass name) ifFalse: [
			^self error: 'Invalid Canvas class' ].
	ActiveSubclass := aMorphicCanvasSubclass.
	UISupervisor ui ifNotNil: [ :world |
		world whenUIinSafeState: [
			UISupervisor newUIProcess ]].! !

!methodRemoval: WorldMorph #setCanvas: stamp: 'jmv 10/10/2023 17:21:51'!
WorldMorph removeSelector: #setCanvas:!
!methodRemoval: WorldMorph #tryDeferredUpdatingAndSetCanvas stamp: 'jmv 10/10/2023 17:21:51'!
WorldMorph removeSelector: #tryDeferredUpdatingAndSetCanvas!
!methodRemoval: WorldMorph #setMainCanvas stamp: 'jmv 10/10/2023 17:21:51'!
WorldMorph removeSelector: #setMainCanvas!
!methodRemoval: WorldMorph #ensureNonDisplayCanvas stamp: 'jmv 10/10/2023 17:21:51'!
WorldMorph removeSelector: #ensureNonDisplayCanvas!
