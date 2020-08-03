'From Cuis 5.0 [latest update: #4272] on 22 July 2020 at 2:57:43 pm'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'world form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine clippingMorphDisplayBounds '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 7/22/2020 14:51:50'!
newClipRect: aRectangleOrNil
	"aRectangle is in world coordinates.
	But ivar clipRect is relative to the form,
	For example, if we had previously been built like
		aCanvas on: someForm over: (100@100 extent 200@100)
	then our origin would be -100 @ -100.
	Then, a clipRect argument like (120@120 extent: 40@30) would mean affecting
	only (20@20 extent: 40@30) in our form"

	self setClipRect: (aRectangleOrNil
		ifNil: [ `0@0` corner: form extent ]
		ifNotNil: [ aRectangleOrNil translatedBy: self canvasOrigin ]).
	clippingMorphDisplayBounds _ nil.! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 7/22/2020 14:47:35'!
clippingByCurrentMorphDo: aBlock
	| prevClipRect prevClippingMorphRect |

	prevClipRect _ self clipRect.
	prevClippingMorphRect _ clippingMorphDisplayBounds.
	"Might use the fact that currentMorph has just been drawn."
	clippingMorphDisplayBounds _ self boundingRectOfCurrentMorphAfterDraw.
	self setClipRect: (prevClipRect intersect: clippingMorphDisplayBounds).
	self clippingMorph: currentMorph.
	aBlock ensure: [
		self clippingMorph: nil.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds _ prevClippingMorphRect ]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/22/2020 14:51:24'!
drawWorld: aPasteUpMorph repair: aDamageRecorder
	"Redraw the damaged areas of the given canvas and clear the damage list.
	Return a collection of the areas that were redrawn.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| rectsToRepair previousRectsToRepair reuse previousCount morphBounds |

"quizas llamarlo #findBoundsDo: o algo asi"
	self updatingMorphBoundsDo: [
		aPasteUpMorph submorphsDo: [ :morph |
			self fullAddRedrawRect: morph to: aDamageRecorder ].
	].

	"The response for #invalidRectsFullBounds: can include nils, that should be ignored."
	rectsToRepair _ aDamageRecorder invalidRectsFullBounds: aPasteUpMorph viewBox.

	rectsToRepair size = 0 ifTrue: [
		^rectsToRepair].

	rectsToRepair size > 1 ifTrue: [									"Or there is no point in attempting to merge them!!"
		previousRectsToRepair _ OrderedCollection new.

		"Until no more rectangles are merged, i.e. each morph intersects at most one rectangle."
		previousCount _ 0.
		[rectsToRepair size = previousCount] whileFalse: [
			previousCount _ rectsToRepair size.
			aPasteUpMorph submorphsReverseDo: [ :morph | | thisMorphRectangle thisMorphBounds |
				morph visible ifTrue: [
					reuse _ previousRectsToRepair.
					previousRectsToRepair _ rectsToRepair.
					rectsToRepair _ reuse removeAll.
					thisMorphRectangle _ nil.
					thisMorphBounds _ morph morphFullBoundsInWorld.
					previousRectsToRepair do: [ :r |
						(thisMorphBounds intersects: r)
							ifTrue: [ thisMorphRectangle _ r quickMerge: thisMorphRectangle ]
							ifFalse: [ rectsToRepair add: r ]].
					thisMorphRectangle
						ifNotNil: [rectsToRepair add: thisMorphRectangle]]]].
	].

	"Draw World"
	rectsToRepair do: [ :r |
		aPasteUpMorph drawOn: (self newClipRect: r) ].

	"Draw morphs"
	aPasteUpMorph submorphsReverseDo: [ :morph |
		morph visible ifTrue: [
			morphBounds _ morph morphFullBoundsInWorld.
			rectsToRepair do: [ :r |
				(morphBounds intersects: r) ifTrue: [
					(self newClipRect: r) fullDraw: morph ]]]].

	^ rectsToRepair! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/22/2020 14:56:35'!
fullAddRedrawRect: aMorph to: aDamageRecorder
	| addMyBounds trySubmorphs |
	aMorph visible ifTrue: [
		addMyBounds _ aMorph valueOfProperty: #needsRedraw ifAbsent: [ false ].
		trySubmorphs _ aMorph valueOfProperty: #submorphNeedsRedraw ifAbsent: [ false ].
		(trySubmorphs or: [addMyBounds]) ifTrue: [
			self into: aMorph.
			addMyBounds ifTrue: [
				currentMorph setProperty: #needsRedraw toValue: false.
				"find bounds.
				agregar mi rect"
				currentMorph drawOn: self.
				world notNil ifTrue: [
					aDamageRecorder recordInvalidRect: self boundingRectOfCurrentMorphAfterDraw ].
			].
			trySubmorphs ifTrue: [
				"llamar recursivo a mis submorphs"
				currentMorph setProperty: #submorphNeedsRedraw toValue: false.
				currentMorph clippedSubmorph ifNotNil: [ :clipped |
					self clippingByCurrentMorphDo: [ self fullAddRedrawRect: clipped to: aDamageRecorder ]].
				currentMorph unclippedSubmorphsReverseDo: [ :m |
					self fullAddRedrawRect: m to: aDamageRecorder ]
			].
			self outOfMorph
		]]! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/22/2020 14:55:26'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates"

	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | self boundingRectOfCurrentMorph intersect: ownerClips ]
		ifNil: [ self boundingRectOfCurrentMorph ]! !

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'world form clipRect transformations currentTransformation cti currentMorph drawingMorphStack engine clippingMorphDisplayBounds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
