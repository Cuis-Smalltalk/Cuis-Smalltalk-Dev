'From Cuis7.5 [latest update: #7279] on 26 June 2025 at 1:00:31 pm'!

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 6/26/2025 12:53:00'!
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
		aMorphicCanvas drawsOnDisplay
			ifTrue: [
				"For instance for Linux and Windows. Drawing was done to Display, but given that #deferUpdates: is on,
				we need to specifically ask for changes to be visually effective."
				DisplayScreen isDisplayExtentOk ifTrue: [
					Display forceToScreen: allDamage ]]
			ifFalse: [
				"For Mac, drawing was done to off-Display canvas. Copy content to Display.
				#deferUpdates: is off, so no additional indication to show Display contents is needed."
				aMorphicCanvas showAt: self viewBox origin invalidRect: allDamage ]].! !


!MorphicCanvas methodsFor: 'testing' stamp: 'jmv 6/26/2025 12:52:04'!
drawsOnDisplay
	"Answer true if we actually draw on (Display shallowCopy)"
	^form == Display | (form bits == Display bits)! !

