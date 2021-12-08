'From Cuis 5.0 [latest update: #4937] on 22 October 2021 at 12:50:57 pm'!

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 10/22/2021 12:34:52'!
externalBoundingRectOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds"

	^Rectangle encompassing: 
		(aRectangle corners
			collect: [ :pt | self transform: pt ])! !


!MorphicTranslation methodsFor: 'transforming rects' stamp: 'jmv 10/22/2021 12:34:55'!
externalBoundingRectOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal and vertical bounds"

	^ aRectangle translatedBy: self translation.! !


!Morph methodsFor: 'change reporting' stamp: 'jmv 10/22/2021 12:47:37'!
invalidateLocalRect: localRectangle

	self
		invalidateDisplayRect:
			(self externalizeBoundsToWorld: localRectangle)
				encompassingIntegerRectangle
		for: self.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/22/2021 12:42:02'!
externalize: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."

	^ aPoint.! !

!Morph methodsFor: 'geometry' stamp: 'jmv 10/22/2021 12:48:55'!
externalizeBoundsToWorld: aRectangle

	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: aRectangle ]
		ifNil: [ aRectangle ]! !


!MovableMorph methodsFor: 'geometry' stamp: 'jmv 10/22/2021 12:42:11'!
externalize: aPoint
	"aPoint is in own coordinates. Answer is in owner's coordinates."

	^ location externalizePosition: aPoint.! !

!MovableMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/22/2021 12:48:44'!
externalizeBoundsToWorld: aRectangle

	| inOwners |
	inOwners _ location externalBoundingRectOf: aRectangle.
	^owner
		ifNotNil: [ owner externalizeBoundsToWorld: inOwners ]
		ifNil: [ inOwners ]! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 10/22/2021 12:48:24'!
externalizeBoundsToWorld: aRectangle

	^ aRectangle! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/22/2021 12:37:52'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known.
	Subclasses may raise this restriction. See inherintance, with VectorGraphics package loaded."

	| boundingRect |
	boundingRect _ (currentTransformation externalBoundingRectOf: currentMorph morphLocalBounds)
		encompassingIntegerRectangle.
	"Include an extra pixel to cover possible anti aliasing."
	boundingRect _ boundingRect outsetBy: 1.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !


!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 10/22/2021 12:39:38'!
ellipseCenter: center radius: radiusPointOrNumber borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |
	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalBoundingRectOf: (Rectangle center: center extent: radiusPointOrNumber asPoint * 2)) rounded.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw rounded].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw rounded) ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/22/2021 12:40:06'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor

	| rect bw |
	r area = 0 ifTrue: [ ^self ].
	rect _ (currentTransformation externalBoundingRectOf: r) rounded.
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/22/2021 12:40:47'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |
	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect _ (currentTransformation externalBoundingRectOf: r) rounded.
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
		self setPaintColor: borderColor.
		engine
			frameRect: rect
			borderWidth: bw ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/22/2021 12:40:52'!
frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)	
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| displayRectangle bw |
	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	displayRectangle _ (currentTransformation externalBoundingRectOf: aRectangle) rounded.
	self
		frameRectangle: displayRectangle
		borderWidth: bw
		topLeftColor: tlColor
		bottomRightColor: brColor.! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 10/22/2021 12:39:50'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule

	| displayRectangle  additionalOffset clippedEngine targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ (currentTransformation externalBoundingRectOf: aRectangle) rounded.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset _ `0@0`.
	clippedEngine _ engine clippedBy: displayRectangle.
	targetTopLeft _ clippedEngine clipRect topLeft truncateTo: ex.
	clipOffset _ displayRectangle topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedEngine clipRect.
	savedMap _ clippedEngine colorMap.
	clippedEngine sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedEngine destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedEngine destOrigin: x@y; copyBits]].
	clippedEngine colorMap: savedMap! !

!methodRemoval: MorphicTranslation #displayBoundsOfTransformOf: stamp: 'jmv 10/22/2021 12:46:37'!
MorphicTranslation removeSelector: #displayBoundsOfTransformOf:!
!methodRemoval: AffineTransformation #displayBoundsOfTransformOf: stamp: 'jmv 10/22/2021 12:46:37'!
AffineTransformation removeSelector: #displayBoundsOfTransformOf:!
!methodRemoval: GeometryTransformation #externalizeRectangle: stamp: 'jmv 10/22/2021 12:46:37'!
GeometryTransformation removeSelector: #externalizeRectangle:!
