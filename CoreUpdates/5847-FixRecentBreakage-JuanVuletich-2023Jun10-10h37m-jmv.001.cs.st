'From Cuis 6.0 [latest update: #5846] on 10 June 2023 at 10:37:33 am'!

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 6/10/2023 10:37:26'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode := self tryDeferredUpdatingAndSetCanvas.

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
	deferredUpdateVMMode ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 6/10/2023 10:37:21'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage damage worldBackgroundRects loopCount |
	visibleRootMorphs := aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage := visibleRootMorphs collect: [ :m | aDamageRecorder damageReportedFor: m ].
	damage := aDamageRecorder damageReportedOther.
	aDamageRecorder reset.
	self gatherMorphDamage: visibleRootMorphs rootsReportedDamage: visibleRootsDamage addDamageTo: damage.
	worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage.
	"It is possible that some morph's damage gets enlarged due to morphs beneath it. If so, act as if the updated morph damage was requested damage from the start, and retry.
	Ocassionally this seemed to loop without bounds. Limit the number of retries."
	loopCount := 0.
	[ (self updateAllDamage: damage roots: visibleRootMorphs rootsDamage: visibleRootsDamage backgroundDamage: worldBackgroundRects) and: [loopCount < 10] ] whileTrue: [
		loopCount := loopCount + 1.
		worldBackgroundRects := self scatterDamageTo: visibleRootMorphs rootsDamageIn: visibleRootsDamage allDamage: damage ].

	self drawWorldBackground: aWorldMorph rects: worldBackgroundRects.
	^ self
		drawRoots: visibleRootMorphs rootsDamage: visibleRootsDamage backgroundDamage: worldBackgroundRects.

	"Debugging aids."
	"
	worldBackgroundRects do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"! !

