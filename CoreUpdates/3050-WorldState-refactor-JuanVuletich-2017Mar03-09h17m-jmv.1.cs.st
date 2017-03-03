'From Cuis 5.0 of 7 November 2016 [latest update: #3049] on 3 March 2017 at 9:17:59 am'!

!WorldState methodsFor: 'stepping' stamp: 'jmv 3/2/2017 19:35:58'!
cleanseStepList
	"Remove morphs from the step list that are not in this World.  Often were in a flap that has moved on to another world."

	| deletions morphToStep |
	deletions _ nil.
	stepList do: [:entry |
		morphToStep _ entry receiver.
		morphToStep world == world ifFalse:[
			deletions ifNil: [deletions _ OrderedCollection new].
			deletions addLast: entry]].

	deletions ifNotNil:[
		deletions do: [:entry|
			self stopSteppingMorph: entry receiver]].

	self alarms copy do:[:entry|
		morphToStep _ entry receiver.
		((morphToStep is: #Morph) and: [ morphToStep world == world ]) 
			ifFalse: [ self removeAlarm: entry selector for: entry receiver ]].! !

!WorldState methodsFor: 'stepping' stamp: 'jmv 3/2/2017 19:42:28'!
runStepMethods
	"Perform periodic activity inbetween event cycles"
	| queue msg |

	queue _ self class deferredUIMessages.
	[ (msg _ queue nextOrNil) isNil ] whileFalse: [
		msg value
	].
	self runLocalStepMethods.

	"we are using a normal #step for these now"
	"aWorld allLowerWorldsDo: [ :each | each runLocalStepMethods ]."
! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/2/2017 19:34:41'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[world displayWorld] ifError: [:err :rcvr |
		"Handle a drawing error"
		| errCtx errMorph |
		errCtx _ thisContext.
		[
			errCtx _ errCtx sender.
			"Search the sender chain to find the morph causing the problem"
			[errCtx notNil and: [ (errCtx receiver is: #Morph) not ]] 
				whileTrue:[errCtx _ errCtx sender].
			"If we're at the root of the context chain then we have a fatal drawing problem"
			errCtx ifNil:[^self handleFatalDrawingError: err].
			errMorph _ errCtx receiver.
			"If the morph causing the problem has already the #drawError flag set,
			then search for the next morph above in the caller chain."
			errMorph isKnownFailing
		] whileTrue.
		errMorph drawingFails.
		self clearCanvas.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	]! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/2/2017 19:34:09'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	lastCycleHadAnyEvent
		ifTrue: [
			pause _ 20.				"This value will only be used when there are no more events to serve."
			wait _ 0 ]					"Don't wait"
		ifFalse: [
			pause < 200				"No events processed? Start saving CPU!! But never make the user wait more than 200ms for being responsive again."
				ifTrue: [ pause _ pause * 21//20 ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].

			wait _ waitUntil - Time localMillisecondClock ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait > 0 ifTrue: [
		waitDelay beingWaitedOn
			ifFalse: [ waitDelay setDelay: wait; wait ]
			ifTrue: [
				"If we are called from a different process than that of the main UI, we might be called in the main
				interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
				(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/2/2017 19:34:47'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	"See #eventTickler"
	Cursor currentCursor = Cursor wait ifTrue: [
		Cursor normal activateCursor ].

	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely.

	"Run steps, alarms and deferred UI messages"
	world runStepMethods.

	"Process user input events. Run all event triggered code."
	hadAnyEvent _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ hadAnyEvent | h processEventQueue.
		activeHand _ nil ].

	"The default is the primary hand"
	activeHand _ self hands first.


	^hadAnyEvent! !

!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/2/2017 19:43:12'!
tryDeferredUpdatingAndSetCanvas
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
	"As this is the only sender of #deferUpdates: , this could be done in Morphic or image startup, and never efterwards..."
	(Display deferUpdates: true) ifNil: [
		"deferred updates not supported by the VM, do them in the image!!"
		self ensureNonDisplayCanvas.
		^ false].
	
	"if no canvas, or canvas was offscreen, from a platform that didnt support defers, then fix it"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	properDisplay ifFalse: [
		world morphPosition: 0@0 extent: Display extent.
		self setCanvas: Display getCanvas.
	].
	^ true! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 3/2/2017 19:43:18'!
displayWorldAndSubmorphs: submorphs
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects handsToDraw allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"
	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"repair world's damage on canvas"
	worldDamageRects _ self drawInvalidAreasSubmorphs: submorphs.

	"Check which hands need to be drawn (they are not the hardware mouse pointer)"
	handsToDraw _ self selectHandsToDrawForDamage: worldDamageRects.
	allDamage _ Array streamContents: [ :strm |
		strm nextPutAll: worldDamageRects.
		handsToDraw do: [ :h | 
			h savePatchFrom: canvas appendDamageTo: strm ]].

	"Draw hands (usually carying morphs) onto world canvas"
	canvas newClipRect: nil.
	handsToDraw reverseDo: [ :h | canvas fullDrawHand: h ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		"Drawing was done to off-Display canvas. Copy content to Display"
		canvas showAt: world viewBox origin invalidRects: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	Display forceDamageToScreen: allDamage.

	"Restore world canvas under hands and their carried morphs"
	handsToDraw do: [ :h | h restoreSavedPatchOn: canvas ].! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 3/2/2017 19:40:12'!
drawInvalidAreasSubmorphs: submorphs
	"Redraw the damaged areas of the given canvas and clear the damage list. Return a collection of the areas that were redrawn."

	| initialRectsToRepair currentRectsToRepair newRectsToRepair morphsToDraw rectsForEachMorph thisMorphRects reuse i n morph morphBounds morphClipRect |

	"The simple implementation is slowers as it draws morph areas that will later be covered by other morphs.
	But it works, and it is easier to understand.
	See en.wikipedia.org/wiki/Painter's_algorithm"
	true not ifTrue: [ ^self simpleDrawInvalidAreasSubmorphs: submorphs ].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	initialRectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.
	damageRecorder reset.
	currentRectsToRepair _ OrderedCollection new.
	newRectsToRepair _ OrderedCollection withAll: initialRectsToRepair.
	morphsToDraw _ OrderedCollection new.
	rectsForEachMorph _ OrderedCollection new.
	thisMorphRects _ OrderedCollection new.
	n _ submorphs size.
	i _ 1.

	"The idea here is to iterate morphs front to back, i.e. first the ones on top.
	For each morph, record the rectangles it affects. And if a morph is opaque, remove the area behind it from the list of rectangles to be repaired.
	This means, for example, that morphs completely covered might even not be redrawn.
	this is a huge performance gain when there are many morphs on screen, especially if there are more than ten open windows, and Theme uses opaque colors.
	See 'A reverse painter's algorithm' at en.wikipedia.org/wiki/Painter's_algorithm"
	[ i <= n and: [ newRectsToRepair notEmpty ]] whileTrue: [
		morph _ submorphs at: i.
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			reuse _ currentRectsToRepair.
			currentRectsToRepair _ newRectsToRepair.
			newRectsToRepair _ reuse removeAll.
			currentRectsToRepair do: [ :r |
				(morphBounds intersects: r)
					ifTrue: [
						morphClipRect _ morphBounds intersect: r.
						thisMorphRects add: morphClipRect. "We could perhaps try and join adjacent rectangles in this collection..."
						morph addPossiblyUncoveredAreasIn: r to: newRectsToRepair ]
					ifFalse: [
						newRectsToRepair add: r ]].
			thisMorphRects ifNotEmpty: [
				morphsToDraw add: morph.
				rectsForEachMorph add: thisMorphRects.
				thisMorphRects _ OrderedCollection new.
			]].
		i _ i + 1 ].

	"*make this true to flash damaged areas for testing*"
	Preferences debugShowDamage ifTrue: [ world fillRects: initialRectsToRepair color: Color gray ].

	"Draw world background on those areas that were not completely covered by opaque morphs"
	i > n  ifTrue: [
		newRectsToRepair do: [ :r |
			world drawOn: (canvas newClipRect: r) ]].

	"Now, draw the recorded areas of selected morphs, back to front."
	morphsToDraw with: rectsForEachMorph reverseDo: [ :m :xrects |
		"Here we could think about merging all xrects into just one call...
		This would mean drawing each morph just once.
		But that would mean drawing pixels we were not told to. There could be other morphs in that area that are not even being drawn!!
		See WorldState class >> #experiment1"
"		rr _ nil."
		xrects do: [ :r |
"			rr _ rr ifNil: [ r ] ifNotNil: [ r quickMerge: rr ]."
			(canvas newClipRect: r) fullDraw: m.
		].
"		(canvas newClipRect: rr) fullDraw: m"
		"*make this true to flash damaged areas for testing*"
		Preferences debugShowDamage2 ifTrue: [
			world flashRects: xrects color: Color random ].
	].

	"Answer a list of rectangles to be updated on the Display.
	This usually is not performance critical, drawing morphs is slower than just exposing the Display."
	"What should we force on Display? Whatever was asked? Each small rect that was updated? A single bigger rect?
	Right now, answer whatever was asked... Maybe this could be changed if that enhances performance...
	(think of vnc over slow networks)"
	^ initialRectsToRepair! !

!WorldState methodsFor: 'drawing' stamp: 'jmv 3/2/2017 19:39:56'!
simpleDrawInvalidAreasSubmorphs: submorphs

	"mover todo esto al canvas, no?
	Y ver que hacer con los argumentos, etc.... Toda esta bananarama!!"

	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	This simple implementation just does as requested: No optimizations are done."

	| rectsToRepair morphBounds |
	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ damageRecorder invalidRectsFullBounds: world viewBox.
	damageRecorder reset.

	"Draw World"
	rectsToRepair do: [ :r |
		world drawOn: (canvas newClipRect: r) ].

	"Draw morphs"
	submorphs reverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(canvas newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !

!WorldState methodsFor: 'accessing' stamp: 'jmv 3/2/2017 21:47:15'!
runLocalStepMethods
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| now morphToStep scheduledTime |
	now _ lastCycleTime.
	self triggerAlarmsBefore: now.
	stepList isEmpty 
		ifTrue: [ ^self].
	[ stepList isEmpty not and: [ stepList first scheduledTime <= now ]] 
		whileTrue: [
			lastStepMessage _ stepList removeFirst.
			morphToStep _ lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: world) 
				ifTrue: [
					scheduledTime _ lastStepMessage scheduledTime.
					lastStepMessage valueAtTime: now - scheduledTime + lastStepMessage stepTime.
					lastStepMessage ifNotNil: [
							lastStepMessage scheduledTime: (scheduledTime + lastStepMessage stepTime max: now + 1).
							stepList add: lastStepMessage ]].
			lastStepMessage _ nil ]! !


!PasteUpMorph methodsFor: 'interaction loop' stamp: 'jmv 3/2/2017 19:34:01'!
doOneCycleNow
	"see the comment in doOneCycleNowFor:
	Only used for a few tests."
	worldState doOneCycleNow! !

!PasteUpMorph methodsFor: 'stepping' stamp: 'jmv 3/2/2017 19:36:23'!
cleanseStepList
	"Remove morphs from the step list that are not in this World.  Often were in a flap that has moved on to another world."

	worldState cleanseStepList! !

!PasteUpMorph methodsFor: 'stepping' stamp: 'jmv 3/2/2017 19:42:32'!
runStepMethods

	worldState runStepMethods! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 3/2/2017 19:34:54'!
displayWorldSafely

	worldState displayWorldSafely
! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 3/2/2017 19:33:20'!
doOneCycle
	"see the comment in WorldState >> doOneCycle"

	worldState doOneCycle! !

!PasteUpMorph methodsFor: 'world state' stamp: 'jmv 3/2/2017 19:37:11'!
privateOuterDisplayWorld

	worldState displayWorldAndSubmorphs: submorphs
! !


!WorldState class methodsFor: 'sample code' stamp: 'jmv 3/2/2017 19:39:21'!
experiment1
	"To play with and learn about morphic Display update logic.
	Remove the morphs when done!!"
	"
	WorldState experiment1
	"
	| dr morph1 morph2 s w |
	morph1 _ RectangleLikeMorph new openInWorld.
	morph2 _ RectangleLikeMorph new openInWorld.
	morph2 color: Color green.
	morph2 morphPosition: 200@800 extent: 50@40.
	morph1 morphPosition: 120@720 extent: 220@100.

	w _ self runningWorld.
	s _ w instVarNamed: 'worldState'.
	dr _ s instVarNamed: 'damageRecorder'.

	dr doFullRepaint.
	dr reset;
	"	recordInvalidRect: (100@700 corner: 400@900);"
		recordInvalidRect: (100@700 corner: 150@900);
		recordInvalidRect: (300@700 corner: 400@900).
	Display fillColor: Color gray.
	"Do not draw the area used by the small rectangle, unless it is also drawn!!"
	s drawInvalidAreasSubmorphs: {morph1. morph2 }! !

!methodRemoval: WorldState #cleanseStepListForWorld:!
WorldState removeSelector: #cleanseStepListForWorld:!
!methodRemoval: WorldState #displayWorld:submorphs:!
WorldState removeSelector: #displayWorld:submorphs:!
!methodRemoval: WorldState #displayWorldSafely:!
WorldState removeSelector: #displayWorldSafely:!
!methodRemoval: WorldState #doOneCycleFor:!
WorldState removeSelector: #doOneCycleFor:!
!methodRemoval: WorldState #doOneCycleNowFor:!
WorldState removeSelector: #doOneCycleNowFor:!
!methodRemoval: WorldState #drawInvalidAreasWorld:submorphs:!
WorldState removeSelector: #drawInvalidAreasWorld:submorphs:!
!methodRemoval: WorldState #runLocalStepMethodsIn:!
WorldState removeSelector: #runLocalStepMethodsIn:!
!methodRemoval: WorldState #runStepMethodsIn:!
WorldState removeSelector: #runStepMethodsIn:!
!methodRemoval: WorldState #simpleDrawInvalidAreasWorld:submorphs:!
WorldState removeSelector: #simpleDrawInvalidAreasWorld:submorphs:!
!methodRemoval: WorldState #tryDeferredUpdatingAndSetCanvasFor:!
WorldState removeSelector: #tryDeferredUpdatingAndSetCanvasFor:!
