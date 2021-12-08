'From Cuis 5.0 [latest update: #4912] on 12 October 2021 at 6:34:24 pm'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/12/2021 18:29:15'!
fullDrawHandOn: aCanvas
	"A HandMorph has an unusual drawing requirement:
		The hand itself (i.e., the cursor) appears in front of its submorphs
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"
	lastPosition _ self morphPosition. "We already know we are carrying morphs."! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/12/2021 18:34:03'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds submorphBounds |
	handBounds _ self displayBounds.
	submorphBounds _ nil.
	self submorphsDo: [ :m |
		submorphBounds _ m displayFullBounds quickMerge: submorphBounds ].
	fullBounds _ handBounds quickMerge: submorphBounds.
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ fullBounds quickMerge: (submorphBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/12/2021 18:30:46'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware
			ifTrue: [
				Preferences cacheDisplayContentWhenMovingMorphs
					ifTrue: [
						"We are caching whatever is in the Display below us. Thefore, there's no need
						to do an invalidation that would trigger the redraw of everything below us."
						self needsRedraw: true ]
					ifFalse:  [
						"No caching of stuff below us. Just invalidate and redraw."
						self redrawNeeded ]]
			ifFalse: [
				lastPosition _ nil.		"Not nil if carrying morphs at that moment"
				prevFullBounds _ nil "Any saved patch is no longer relevant"]].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/12/2021 18:28:52'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld _ aMorph morphPositionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		formerPositionInWorld ifNotNil: [
			d _ aMorph morphPositionInWorld - formerPositionInWorld.
			d isZero ifFalse: [
				aMorph adjustDisplayBoundsBy: d ]]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		"Morph was in the world"
		lastPosition _ self morphPosition ].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/12/2021 18:10:26'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	self hideHardwareCursor.
	self redrawNeeded.

	moveUnderHand ifTrue: [
		bounds _ nil.
		grabbed isInWorld ifTrue: [
			grabbed displayBounds ifNotNil: [ :r |
				bounds _ r translatedBy: grabbed morphPositionInWorld negated ]].
		bounds ifNil: [ bounds _ grabbed findFullBoundsInOwner ].
		positionInHandCoordinates _ (bounds center + bounds bottomRight //2) negated.
		self grabMorph: grabbed delta: positionInHandCoordinates.
		^self ].

	positionInHandCoordinates _ (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		morphPositionInWorld - self morphPositionInWorld.

	tx _ GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx _ o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx _ o location inverseTransformation composedWith: tx ].
	self grabMorph: grabbed delta: positionInHandCoordinates.

	grabbed location: tx.! !


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/12/2021 16:41:47'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	Preferences cacheDisplayContentWhenMovingMorphs ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	canvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage _ canvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		Preferences cacheDisplayContentWhenMovingMorphs
			ifTrue: [ allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage _ h displayFullBoundsForPatch quickMerge: allDamage ].
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


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/12/2021 16:41:53'!
drawWorld: aWorldMorph repair: aDamageRecorder
	"Redraw the damaged areas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| visibleRootMorphs visibleRootsDamage worldBackgroundRects |
	visibleRootMorphs _ aWorldMorph privateSubmorphs select: [ :m | m visible ].
	visibleRootsDamage _ Array new: visibleRootMorphs size.

	worldBackgroundRects _ self computeDamage: aWorldMorph repair: aDamageRecorder
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage.
	aDamageRecorder reset.

	self drawWorldBackground: aWorldMorph rects: worldBackgroundRects.
	"Debugging aids."
	"
	worldBackgroundRects do: [ :r | Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.005) wait].
	""
	visibleRootsDamage do: [ :r | r ifNotNil: [ Display fill: r fillColor: Color random . Display forceToScreen. (Delay forSeconds: 0.05) wait]].
	"

	^ self drawWorld: aWorldMorph
		rootMorphs: visibleRootMorphs rootMorphsDamage: visibleRootsDamage
		backgroundDamage: worldBackgroundRects.! !

