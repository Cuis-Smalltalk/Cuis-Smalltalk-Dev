'From Cuis 6.0 [latest update: #5713] on 22 March 2023 at 11:27:19 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/22/2023 11:19:55'!
boundingRectOfCurrentIfTrivial
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	currentMorph knowsOwnLocalBounds ifTrue: [
		^ (currentTransformation externalBoundingRectOf: currentMorph morphLocalBounds)
			encompassingIntegerRectangle ].
	^nil.! !


!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 3/22/2023 11:19:47'!
boundingRectOfCurrentIfTrivial
	"In targetForm coordinates.
	Answer only if it can be known wothout drawing.
	Don't include any clipping."

	| boundingRect |
	currentMorph knowsOwnLocalBounds ifTrue: [
		boundingRect :=  (currentTransformation externalBoundingRectOf: currentMorph morphLocalBounds)
			encompassingIntegerRectangle.
		"Include an extra pixel to cover possible anti aliasing."
		currentMorph requiresVectorCanvas ifTrue: [
			boundingRect := boundingRect outsetBy: 1 ].
		^boundingRect ].
	^nil.! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 3/22/2023 08:23:56'!
drawClassicFrameOn: aCanvas color: windowFrameColor
	"Window border encompasses title area. No round corners."

	aCanvas
		fillRectangle: (self morphLocalBounds insetBy: 1.5)
		color: color
		borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self borderColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas
		fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight)
		color: windowFrameColor.! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/22/2023 10:44:38'!
drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: strict
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in visibleTextBounds"

	| displayScanner leftInRun line possiblyVisible firstLineIndex lastLineIndex |

	possiblyVisible := currentTransformation boundsOfInverseTransformOf: clipRect.
	possiblyVisible := (possiblyVisible translatedBy: drawingPosition negated) intersect: (0@0 extent: anExtent).

	displayScanner := MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun := 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	firstLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible topLeft max: `0@0`).
	lastLineIndex := aTextComposition lineIndexForPoint: (possiblyVisible bottomRight min: anExtent).
	firstLineIndex
		to: lastLineIndex
		do: [ :i |
			line := aTextComposition lines at: i.
			(strict not or: [ line top + line baseline < possiblyVisible bottom ]) ifTrue: [
				aTextComposition
					displaySelectionInLine: line
					on: self
					textTopLeft: drawingPosition
					selectionColor: sc.
				leftInRun := displayScanner displayLine: line textTopLeft: drawingPosition leftInRun: leftInRun ]]! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 3/22/2023 10:44:33'!
clippingByCurrentMorphDo: aBlock
	"Do clipping only if currentMorph says so"

	| prevClipRect prevClippingMorphRect |

	currentMorph clipsSubmorphsReally ifFalse: [ ^aBlock value ].

	prevClipRect := clipRect.
	prevClippingMorphRect := clippingMorphDisplayBounds.
	clippingMorphDisplayBounds := self boundingRectOfCurrentMorphAfterDraw insetBy: 1.
	self setClipRect: (prevClipRect
		ifNil: [clippingMorphDisplayBounds]
		ifNotNil: [prevClipRect intersect: clippingMorphDisplayBounds]).
	self clipCurrentMorph: true.
	aBlock ensure: [
		self clipCurrentMorph: false.
		self setClipRect: prevClipRect.
		clippingMorphDisplayBounds := prevClippingMorphRect ]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 3/22/2023 11:21:23'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid for morphs where #morphLocalBounds is known.
	Subclasses may raise this restriction. See inherintance."

	| boundingRect |
	boundingRect := self boundingRectOfCurrentIfTrivial.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !


!AbstractVectorCanvas methodsFor: 'morphic world' stamp: 'jmv 3/22/2023 11:22:42'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	Only valid _after_ drawing currentMorph.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used only as an optimization of the area to be redrawn."

	| boundingRect |
	"A good optimization in many cases.
	But for morphs where #drawOn: might not be able to find bounds (InnerTextMorph and LayoutMorph,
	for example) this is essential."
	self boundingRectOfCurrentIfTrivial ifNotNil: [ :br |
		^ clippingMorphDisplayBounds
			ifNotNil: [ :ownerClips | br intersect: ownerClips ]
			ifNil: [ br ]].

	"Use the bounds bound by the engine during #drawOn.
	This method must therefore be called after calling #drawOn:."
	boundingRect := engine morphBoundsAfterDraw.
	^ clippingMorphDisplayBounds
		ifNotNil: [ :ownerClips | boundingRect intersect: ownerClips ]
		ifNil: [ boundingRect ]! !


!BoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 3/22/2023 10:43:25'!
drawCurrentAndSubmorphs
	
	currentMorph visible ifFalse: [ ^self ].

	currentMorph drawOn: self.
	self displayBoundsSetOnCurrentMorph.

	"Display submorphs back to front"
	"coordinate system stack already set up for aMorph
	('ivars transformations' and 'currentTransformation')"
	self clippingByCurrentMorphDo: [
		currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

	(currentMorph postDrawOn: self) ifTrue: [
		self displayBoundsUpdateOnCurrentMorph ].

	currentMorph isHighlighted ifTrue: [
		self drawHighlight: currentMorph ].! !


!VectorCanvas methodsFor: 'morphic' stamp: 'jmv 3/22/2023 11:24:32'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := true.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				currentMorph submorphsMightProtrude ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.
		self contourSetOnCurrentMorph.
		self contourUpdateOnAllOwners ].

	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(currentMorph postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph.
			self contourUpdateOnCurrentMorph.
			self contourUpdateOnAllOwners ].
		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ]].! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 3/22/2023 11:24:27'!
drawCurrentAndSubmorphs

	| currentNeedsDrawing submorphsNeedDrawing |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := true.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				currentMorph submorphsMightProtrude ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph ].

	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(currentMorph postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph ].
		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ]].! !


!HybridCanvas methodsFor: 'morphic' stamp: 'jmv 3/22/2023 11:23:23'!
drawCurrentAndSubmorphs
	"If we get here, we are drawing with BitBlt, not VectorCanvas.
	Clear morphIds of pixels drawn for currentMorph"

	| currentNeedsDrawing submorphsNeedDrawing |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := true.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				currentMorph submorphsMightProtrude ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self onMorphIdsDo: [
			currentMorph drawOn: self ].
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph ].

	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(currentMorph postDrawOn: self) ifTrue: [
			self onMorphIdsDo: [
				currentMorph postDrawOn: self ].
			self displayBoundsUpdateOnCurrentMorph ].
		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ]].! !

