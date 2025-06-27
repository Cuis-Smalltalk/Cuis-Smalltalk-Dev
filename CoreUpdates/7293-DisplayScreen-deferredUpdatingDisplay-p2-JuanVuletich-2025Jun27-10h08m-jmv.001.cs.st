'From Cuis7.5 [latest update: #7289] on 27 June 2025 at 10:25:25 am'!

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 6/27/2025 10:22:57'!
forceDeferredToScreen
	"Force the entire display area to the screen"
	DeferredUpdatingDisplay ifNotNil: [
		self forceDeferredToScreen: DeferredUpdatingDisplay boundingBox ]! !

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 6/27/2025 10:20:09'!
forceDeferredToScreen: updateRect
	"Force the given rectangular section of the Display to be copied to the host screen. Make it visible to user."

	DeferredUpdatingDisplay ifNil:
		[ ^self ].
	DisplayScreen isDisplayExtentOk ifFalse:
		[ ^self ].
	DeferredUpdatingDisplay == Display
		ifTrue:
			[Display forceToScreen: updateRect ]
		ifFalse:
			[
			(BitBlt toForm: Display)
				sourceForm: DeferredUpdatingDisplay;
				combinationRule: Form over;
				sourceRect: updateRect;
				destOrigin: updateRect topLeft;
				copyBits ]! !


!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 6/27/2025 10:10:40'!
deferredUpdatingDisplay
	"Answer the instance to be used from most clients, including Morphic.
	If the platform supports #deferUpdates:
		Set Display to deferred upodating
		Answer will be Display itself.
	If the platform does NOT support #deferUpdates
		Answer a separate Form.
	Methods in this category provide convenient access."

	DeferredUpdatingDisplay ifNil: [
		 DeferredUpdatingDisplay := (Display deferUpdates: true)
			"Both Linux and Windows VM do support deferUpdates:, so Morphic Canvas target is Display."
			ifNotNil: [ Display ]
			"Mac VMs don't support deferUpdates:, so Morphic Canvas target is this aux Form."
			ifNil: [ Form extent: Display extent depth: Display depth ]].
	^DeferredUpdatingDisplay! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 6/27/2025 10:20:32'!
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
	self submorphNeedsRedraw: false.
	self needsRedraw: false.
	aMorphicCanvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self ifHandToBeDrawnForDamage: allDamage do: [
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage := (activeHand savePatchFrom: aMorphicCanvas) quickMerge: allDamage ]
			ifFalse: [ allDamage := activeHand displayFullBoundsForPatch quickMerge: allDamage ].
		aMorphicCanvas fullDrawHand: activeHand.
		activeHand needsRedraw: false.
		activeHand submorphNeedsRedraw: false ].

	"Copy altered rects to Display"
	allDamage ifNotNil: [
		DisplayScreen forceDeferredToScreen: allDamage ].! !

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 6/27/2025 10:24:45'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			super drawOn: aCanvas ]
		ifNil: [
			"draw background fill"
			(aCanvas form == Display and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: `0@0` in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]! !


!MorphicCanvas class methodsFor: 'instance creation' stamp: 'jmv 6/27/2025 10:11:44'!
onDisplay
	"Answer an instance of the appropriate subclass targetting Display,
	or an intermediate buffer, as appropriate.
	Create a new VectorEngineDrawer or BitBltCanvasEngine as appropriate.
	Use a#deferredUpdatingDisplay, so actual contents are flushed to visible pixels only when appropriate.
	See DisplayScreen 'deferred updating' class methods category."

	| canvasTarget |
	canvasTarget := DisplayScreen deferredUpdatingDisplay.
	^self onForm: canvasTarget.! !

!methodRemoval: MorphicCanvas #drawsOnDisplay stamp: 'jmv 6/27/2025 10:24:49'!
MorphicCanvas removeSelector: #drawsOnDisplay!
!methodRemoval: DisplayScreen class #forceToScreen stamp: 'jmv 6/27/2025 10:23:00'!
DisplayScreen class removeSelector: #forceToScreen!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor ui releaseCachedState.
UISupervisor spawnNewMorphicProcessFor: UISupervisor ui

