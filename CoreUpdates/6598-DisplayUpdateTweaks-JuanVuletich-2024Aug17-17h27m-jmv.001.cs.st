'From Cuis7.1 [latest update: #6596] on 17 August 2024 at 5:29:33 pm'!

!DisplayScreen methodsFor: 'other' stamp: 'jmv 8/16/2024 16:41:15'!
deferUpdates: aBoolean
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen.  To actually make them visible, call #forceToScreen: .
	If this underlying platform does not support deferred updates, this primitive will fail. Answer  nil if it fails.
	Most platforms do support this functionality. You can turn it off for playing with Display directly."

	"Enable this to act as if the VM didn't support defer updates, even when it does"
	"true ifTrue: [ ^nil ]."

	"Note: If we disable VM defer updates (with this &false), but answer notNil, the the Morphic workaround is not used,
	and you get a lot of flicking."
	"As of as August 2024, the Mac VM (intel) doesn't support deferred updates and uses this auxiliar Form as target.
	Both Linux and Windows VM do support it, so Morphic Canvas target is Display."
	^self primitiveDeferUpdates: aBoolean "& false"! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 8/16/2024 16:47:38'!
displayWorldOn: aMorphicCanvas
	"Update this world's display on the argument."

	| allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"Restore world drawing under hands and their carried morphs"
	(Preferences at: #cacheDisplayContentWhenMovingMorphs) ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: aMorphicCanvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	aMorphicCanvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage := aMorphicCanvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	aMorphicCanvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		(Preferences at: #cacheDisplayContentWhenMovingMorphs)
			ifTrue: [ allDamage := (h savePatchFrom: aMorphicCanvas) quickMerge: allDamage ]
			ifFalse: [ allDamage := h displayFullBoundsForPatch quickMerge: allDamage ].
		aMorphicCanvas fullDrawHand: h .
		h needsRedraw: false ].

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

