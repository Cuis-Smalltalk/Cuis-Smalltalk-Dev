'From Cuis7.1 [latest update: #6674] on 4 September 2024 at 2:56:15 pm'!

!WorldMorph methodsFor: 'hands' stamp: 'jmv 9/4/2024 14:41:51'!
ifHandToBeDrawnForDamage: aRectangle do: aBlock
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	activeHand isDrawnBySoftware ifTrue: [
		activeHand isRedrawNeeded
			ifTrue: aBlock
			ifFalse: [
				aRectangle ifNotNil: [
					activeHand displayFullBounds ifNotNil: [ :handBounds |
						(aRectangle intersects: handBounds) ifTrue: aBlock ]]]].! !


!HandMorph methodsFor: 'halo handling' stamp: 'jmv 9/4/2024 14:36:13'!
obtainHalo: aHalo
	"Used for transfering halos between hands"

	self halo == aHalo ifTrue: [^self].
	"Find former owner"
	self world activeHand halo == aHalo ifTrue: [
		self world activeHand releaseHalo: aHalo].
	self halo: aHalo.! !


!WorldMorph methodsFor: 'caching' stamp: 'jmv 9/4/2024 14:56:01'!
releaseCachedState
	super releaseCachedState.
	self cleanseStepList.
	canvas ifNotNil: [
		canvas unpinAndFreeMemory ].
	canvas := nil.
	damageRecorder := nil.
	activeHand releaseCachedState.
	hands := nil.! !

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 9/4/2024 14:42:15'!
displayWorldOn: aMorphicCanvas
	"Update this world's display on the argument."

	| allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"Restore world drawing under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		activeHand restoreSavedPatchOn: aMorphicCanvas ].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	aMorphicCanvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage := aMorphicCanvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	aMorphicCanvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self ifHandToBeDrawnForDamage: allDamage do: [
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage := (activeHand savePatchFrom: aMorphicCanvas) quickMerge: allDamage ]
			ifFalse: [ allDamage := activeHand displayFullBoundsForPatch quickMerge: allDamage ].
		aMorphicCanvas fullDrawHand: activeHand.
		activeHand needsRedraw: false ].

	"Copy altered rects to Display"
	allDamage ifNotNil: [
		aMorphicCanvas drawsOnDisplay
			ifTrue: [
				"For instance for Linux and Windows. Drawing was done to Display, but given that #deferUpdates: is on,
				we need to specifically ask for changes to be visually effective."
				DisplayScreen isDisplayExtentOk ifTrue: [
					Display forceToScreen: allDamage ]]
			ifFalse: [
				"For Mac, drawing was done to off-Display canvas. Copy content to Display.
				#deferUpdates: is off, so no additional indication to show Display contents is needed."
				aMorphicCanvas showAt: self viewBox origin invalidRect: allDamage]].! !

!WorldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/4/2024 14:36:26'!
deleteBalloonTarget: aMorph
	"Delete the balloon help targeting the given morph"
	activeHand deleteBalloonTarget: aMorph.! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 9/4/2024 14:31:14'!
activeHand
	^activeHand! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 9/4/2024 14:31:23'!
initialize

	super initialize.
	activeHand := HandMorph new.
	damageRecorder := DamageRecorder new.
	stepList := Heap sortBlock: self stepListSortBlock.
	alarms := Heap sortBlock: self alarmSortBlock.
	lastAlarmTime := 0.
	deferredUIMessages := SharedQueue new.
	drawingFailingMorphs := WeakIdentitySet new.
	pause := 20.
	lastCycleTime := Time localMillisecondClock.
	lastCycleHadAnyEvent := false! !

!WorldMorph methodsFor: 'misc' stamp: 'jmv 9/4/2024 14:37:24'!
restoreDisplay
	self morphExtent: Display extent.
	activeHand visible: true.
	self fullRepaintNeeded.! !

!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/4/2024 14:38:36'!
allMorphsDo: aBlock
	"Enumerate all morphs in the world, including those held in hands."

	super allMorphsDo: aBlock.
	activeHand allMorphsDo: aBlock.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/4/2024 14:34:02'!
checkIfUpdateNeeded

	self isSubmorphRedrawNeeded ifTrue: [ ^true ].
	damageRecorder updateIsNeeded ifTrue: [^true].
	(activeHand isRedrawNeeded | activeHand isSubmorphRedrawNeeded and: [
		activeHand isDrawnBySoftware]) ifTrue: [^true].
	^false  "display is already up-to-date"! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/4/2024 14:39:07'!
doOneCycleNowOn: aMorphicCanvas
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	lastCycleTime := Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldOn: aMorphicCanvas.
	"Run steps, alarms and deferred UI messages."
	self runDeferredUIMessages.
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.
	"Ensure that any affected morph is redrawn ASAP."
	self displayWorldOn: aMorphicCanvas.
	"Process user input events. Run all event triggered code."
	hadAny := activeHand processEventQueue.
	"Run any deferred UI messages that were enqueued during this cycle, perhaps
	as consequence of event handling or step methods, or sent from other processes,
	before the inter cycle pause."
	self runDeferredUIMessages.
	"Ensure that any affected morph is redrawn ASAP, before the inter cycle pause."
	self displayWorldOn: aMorphicCanvas.
	lastCycleHadAnyEvent := hadAny.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 9/4/2024 14:32:44'!
doOneCycleOn: aMorphicCanvas delay: delay
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta := Time localMillisecondClock - lastCycleTime.
	r := 15@1515 extent: 60@30.
	Delta := Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	self doOneCycleNowOn: aMorphicCanvas.

	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause := 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait := 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			activeHand waitingForMoreClicks
				ifTrue: [ pause := 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause := pause * 21//20 ] ].
			waitUntil := lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil := waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil := waitUntil min: alarms first scheduledTime ]].
			wait := waitUntil - Time localMillisecondClock max: 0 ].
	(Preferences at: #serverMode)
		ifTrue: [ wait := wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			delay setDelay: wait; wait ].! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 9/4/2024 14:36:51'!
haloMorphsDo: aBlock
	activeHand halo ifNotNil: [ :halo | aBlock value: halo ].! !


!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 9/4/2024 14:35:16'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w := self new.
	w color: (Color r: 0.167 g: 0.344 b: 0.629).
	w borderWidth: 0.
	w position: `0@0` extent: Display extent.
	w activeHand privateOwner: w.
	^w! !


!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 9/4/2024 14:38:02'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."

	| hand |
	hand := aWorldMorph activeHand.
	hand hasSubmorphs ifTrue: [
		self into: hand.
		hand submorphsDo: [ :morph |
			morph basicDisplayBounds ifNil: [
				self fullUpdateProtrudingBounds: morph ]].
		self outOfMorph ].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 9/4/2024 14:38:15'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."

	| hand |
	hand := aWorldMorph activeHand.
	hand hasSubmorphs ifTrue: [
		self into: hand.
		hand submorphsDo: [ :morph |
			morph basicDisplayBounds ifNil: [
				self fullUpdateProtrudingBounds: morph ]].
		self outOfMorph ].! !

!methodRemoval: WorldMorph #handsToDrawForDamage:do: stamp: 'jmv 9/4/2024 14:55:34'!
WorldMorph removeSelector: #handsToDrawForDamage:do:!
!methodRemoval: WorldMorph #handsDo: stamp: 'jmv 9/4/2024 14:55:34'!
WorldMorph removeSelector: #handsDo:!
!methodRemoval: WorldMorph #hands stamp: 'jmv 9/4/2024 14:55:34'!
WorldMorph removeSelector: #hands!
!methodRemoval: WorldMorph #handsReverseDo: stamp: 'jmv 9/4/2024 14:55:34'!
WorldMorph removeSelector: #handsReverseDo:!
