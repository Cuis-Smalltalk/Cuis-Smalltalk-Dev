'From Cuis 5.0 [latest update: #4298] on 26 July 2020 at 7:51:01 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/26/2020 19:46:44'!
restorePatch: savedPatch
	"Argument must previously be obtained by calling #savePatch:bounds:
	Senders should not assume anything about the returned object, except that it will not be nil."

	self
		image: savedPatch
		at: savedPatch offset.! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/26/2020 19:45:54'!
savePatch: prevSavedPatch bounds: aRectangle
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch bb |
	savedPatch _ prevSavedPatch.
	(savedPatch isNil or: [savedPatch extent ~= aRectangle extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: aRectangle extent depth: form depth ].
	bb _ BitBlt toForm: savedPatch.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	savedPatch offset: aRectangle topLeft.
	^savedPatch! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/26/2020 19:39:19'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."
	hasChanged _ false.
	savedPatch ifNotNil: [
		aCanvas restorePatch: savedPatch.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self
			invalidateDisplayRect: (self morphPosition extent: extent)
			from: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		savedPatch _ nil ].! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/26/2020 19:44:48'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	| fullBounds |
	fullBounds _ self displayFullBounds ifNil: [ ^self ].
	fullBounds _ fullBounds intersect: (`0@0` extent: aCanvas extent).
	savedPatch _ aCanvas savePatch: savedPatch bounds: fullBounds.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds quickMerge: prevFullBounds)].
	prevFullBounds _ fullBounds! !


!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 7/26/2020 19:40:48'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |

	engine ifNil: [ ^nil ].

	p _ (currentTransformation transform: aPoint) rounded.
	r _ (form depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	engine colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	engine image: aForm at: p sourceRect: sourceRect rule: r.
	(form depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!!)
		This is the same workaround as in #asFormOfDepth:"
		engine sourceForm: nil.
		engine combinationRule: 40. "fixAlpha:with:"
		engine copyBits ]! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 7/26/2020 19:40:38'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ aColor ifNil: [ `Color transparent` ].
	(paintColor is: #Color) ifFalse: [
		^ self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
	engine sourceForm: nil.
	(paintColor isOpaque or: [ form depth < 32]) ifTrue: [
		engine fillColor: paintColor.
		engine combinationRule: Form paint.
		^ self ].

	"BitBlt setup for alpha mapped transfer"
	engine fillColor: paintColor.
	engine combinationRule: Form blend! !

!methodRemoval: MorphicCanvas #depth stamp: 'jmv 7/26/2020 19:42:07'!
MorphicCanvas removeSelector: #depth!
!methodRemoval: MorphicCanvas #contentsOfArea:into: stamp: 'jmv 7/26/2020 19:43:09'!
MorphicCanvas removeSelector: #contentsOfArea:into:!
!methodRemoval: Form #contentsOfArea:into: stamp: 'jmv 7/26/2020 19:43:36'!
Form removeSelector: #contentsOfArea:into:!
