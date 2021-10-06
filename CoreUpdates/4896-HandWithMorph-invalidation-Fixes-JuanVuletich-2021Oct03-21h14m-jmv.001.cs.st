'From Cuis 5.0 [latest update: #4895] on 3 October 2021 at 9:16:42 pm'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/3/2021 19:51:20'!
hideHardwareCursor

	| blankCursor |
	blankCursor _ Cursor cursorAt: #blankCursor.
	Cursor currentCursor == blankCursor ifFalse: [
		blankCursor activateCursor ].! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/3/2021 20:58:03'!
savePatchFrom: aCanvas
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	| fullBounds answer |
	fullBounds _ self displayFullBoundsForPatch intersect: (`0@0` extent: aCanvas extent).
	savedPatch _ aCanvas savePatch: savedPatch bounds: fullBounds.
	answer _ fullBounds.
	prevFullBounds ifNotNil: [ answer _ answer quickMerge: prevFullBounds ].
	prevFullBounds _ fullBounds.
	^answer! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/3/2021 20:22:25'!
showHardwareCursor

	"Make the transition to using hardware cursor.
	Report one final damage rectangle to erase the image of the software cursor."
	Cursor currentCursor == (Cursor cursorAt: #blankCursor) ifTrue: [ 
		"show hardware cursor"
		Cursor defaultCursor activateCursor.
		self invalidateDisplayRect: self basicDisplayBounds for: nil ].! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/3/2021 20:42:10'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware ifTrue: [
			self needsRedraw: true ]].! !


!WorldMorph methodsFor: 'hands' stamp: 'jmv 10/3/2021 21:09:56'!
handsToDrawForDamage: damageList do: aBlock
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	hands do: [: hand |
		hand isDrawnBySoftware ifTrue: [
			hand isRedrawNeeded
				ifTrue: [aBlock value: hand]
				ifFalse: [
					hand displayFullBounds ifNotNil: [ :handBounds |
						(damageList anySatisfy: [ :r | r intersects: handBounds]) ifTrue: [
							aBlock value: hand]]]]].! !


!MorphicCanvas methodsFor: 'other' stamp: 'jmv 10/3/2021 21:03:48'!
showAt: pt invalidRect: updateRect
	| blt |
	blt _ (BitBlt toForm: Display)
		sourceForm: form;
		combinationRule: Form over.
	blt sourceRect: updateRect;
		destOrigin: updateRect topLeft + pt;
		copyBits! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/3/2021 20:21:06'!
isDrawnBySoftware
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor.
	Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"

	^ submorphs anySatisfy: [ :ea | ea visible ]! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 10/3/2021 20:28:53'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch."
	
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		submorphs isEmpty ifTrue: [
			prevFullBounds _ nil ]].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/3/2021 20:22:19'!
dropMorphs: anEvent
	"Drop the morphs at the hands position"

	self submorphsReverseDo: [ :m |
		"Drop back to front to maintain z-order"
		self dropMorph: m event: anEvent ].
	self showHardwareCursor.! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 10/3/2021 19:50:49'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self hideHardwareCursor.
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].

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


!WorldMorph methodsFor: 'drawing' stamp: 'jmv 10/3/2021 21:12:11'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode worldDamageRects allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	hands do: [ :h | h restoreSavedPatchOn: canvas ].

	"repair world's damage on canvas"
	worldDamageRects _ canvas drawWorld: self repair: damageRecorder.
	"worldDamageRects do: [ :r | Display border: r width: 3 fillColor: Color random ].	 'Debugging Aid'."
	allDamage _ Rectangle merging: worldDamageRects.
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: worldDamageRects do: [ :h |
		allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage.
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		"Drawing was done to off-Display canvas. Copy content to Display"
		canvas showAt: self viewBox origin invalidRect: allDamage ].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 10/3/2021 19:28:36'!
displayBoundsForHand: aHand

	^ aHand morphPosition asIntegerPoint - 8 extent: aHand morphExtent.! !

!methodRemoval: MorphicCanvas #showAt:invalidRects: stamp: 'jmv 10/3/2021 21:15:47'!
MorphicCanvas removeSelector: #showAt:invalidRects:!
!methodRemoval: WorldMorph #selectHandsToDrawForDamage: stamp: 'jmv 10/3/2021 21:15:47'!
WorldMorph removeSelector: #selectHandsToDrawForDamage:!
!methodRemoval: HandMorph #savePatchFrom:appendDamageTo: stamp: 'jmv 10/3/2021 21:15:47'!
HandMorph removeSelector: #savePatchFrom:appendDamageTo:!
!methodRemoval: DisplayScreen #forceDamageToScreen: stamp: 'jmv 10/3/2021 21:15:47'!
DisplayScreen removeSelector: #forceDamageToScreen:!
