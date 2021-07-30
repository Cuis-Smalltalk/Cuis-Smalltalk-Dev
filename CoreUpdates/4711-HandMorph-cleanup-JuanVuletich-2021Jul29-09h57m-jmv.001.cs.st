'From Cuis 5.0 [latest update: #4710] on 29 July 2021 at 10:09:16 am'!

!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/29/2021 10:01:46'!
drawOn: aCanvas
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."
	aCanvas
		stencil: (Cursor cursorAt: #moveCursor)
		at: `-8 @ -8`
		color: Color black.! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/29/2021 10:08:07'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	self needsRedraw: false.
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self invalidateDisplayRect: self displayBounds for: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		prevFullBounds _ nil ].! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/29/2021 10:09:03'!
displayBounds

	^ self morphPosition-8 extent: self morphExtent.! !


!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 7/29/2021 09:47:11'!
stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	"
	| dot |
	dot := ((Form dotOfSize: 32) asFormOfDepth: 1) offset: 0@0.
	dot displayAt: 20@20.
	Display getCanvas stencil: dot at: 60@20 color: Color red.
	Display forceToScreen
	"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor! !

!methodRemoval: MorphicCanvas #stencil:at:color: stamp: 'jmv 7/29/2021 09:57:11'!
MorphicCanvas removeSelector: #stencil:at:color:!
!methodRemoval: MorphicCanvas #stencil:at:sourceRect:color: stamp: 'jmv 7/29/2021 09:57:11'!
MorphicCanvas removeSelector: #stencil:at:sourceRect:color:!
