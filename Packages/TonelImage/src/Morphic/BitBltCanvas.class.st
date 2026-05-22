"
origin is aPoint, and it is expressed relative to the form. Usually, the form doesn't cover the whole World, and origin is negative. For instance, if we just want to draw the part of the World in (100@100 extent: 50@50) to another form, then fom extent = 50@50, and origin = -100@ -100.

clipRect is relative to the form. For instance, if we only need to draw the part in (110@110 extent: 20@20) to the form above, then clipRect is (10@10 extent: 20@20)

All public protocol and drawing services is relative to World. The idea is that we only care about origin/form when we set them. Afterwards, we don't need to care about them. All the operations are done as if the whole World was drawn on Display.
"
Class {
	#name : 'BitBltCanvas',
	#superclass : 'MorphicCanvas',
	#instVars : [
		'boundsFinderCanvas'
	],
	#classVars : [
		'AccessProtect',
		'AuxBlitter',
		'AuxForm',
		'CachedForms'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'cached state access' }
BitBltCanvas class >> accessProtect [
	AccessProtect ifNil: [
		AccessProtect := Semaphore forMutualExclusion ].
	^AccessProtect
]

{ #category : 'cached button forms' }
BitBltCanvas class >> arrowOfDirection: aSymbol size: finalSizeInteger [
	^self cachedForms
		at: { aSymbol . finalSizeInteger asInteger}
		ifAbsentPut: [
			self buildArrowOfDirection: aSymbol size: finalSizeInteger ]
]

{ #category : 'cached forms' }
BitBltCanvas class >> bottomLeftCorner: r height: height [
	^self cachedForms
		at: { #bottomLeft. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				bottomLeftCorner: r
				height: height ]
]

{ #category : 'cached forms' }
BitBltCanvas class >> bottomRightCorner: r height: height [
	^self cachedForms
		at: { #bottomRight. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				bottomRightCorner: r
				height: height ]
]

{ #category : 'cached button forms' }
BitBltCanvas class >> buildArrowOfDirection: aSymbolDirection size: finalSizeInteger [
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor  
	 
	aSymbolDirectionDirection = #up, #down. #left or #right
	 (self buildArrowOfDirection: #down size: 120) display
	 (self buildArrowOfDirection: #up size: 120) display
	"

	| resizeFactor resizedForm f e c topLeftMargin bottomRightMargin |
	resizeFactor := 4.
	e := finalSizeInteger@finalSizeInteger*resizeFactor.
	f := Form extent: e depth: 32.
	c := `Color gray: 0.5`.
	topLeftMargin := 0.
	bottomRightMargin := finalSizeInteger*resizeFactor//4.
	topLeftMargin := finalSizeInteger*resizeFactor//4.
	bottomRightMargin := 0.
	(#(up down) includes: aSymbolDirection) 
		ifTrue: [
			topLeftMargin to: e y-1-bottomRightMargin do: [ :y |
				0 to: e x -1 do: [ :x |
					(e x / 2 - 1 - x) abs * 2 + bottomRightMargin< (e y -1- y) ifTrue: [
						f colorAt: x@y put: c ]]]]
		ifFalse: [
			topLeftMargin to: e x-1-bottomRightMargin do: [ :x |
				0 to: e y -1 do: [ :y |
					(e y / 2 - 1 - y) abs * 2 + bottomRightMargin < (e x -1-x) ifTrue: [
						f colorAt: x@y put: c ]]]].
	resizedForm := f
		magnify: f boundingBox
		by: 1 / resizeFactor
		smoothing: 4.
	aSymbolDirection = #up ifTrue: [
		resizedForm := resizedForm flippedBy: #vertical ].
	aSymbolDirection = #left ifTrue: [
		resizedForm := resizedForm flippedBy: #horizontal ].
	^resizedForm
]

{ #category : 'services' }
BitBltCanvas class >> buildAuxWith: aForm multipliedWith: aColor [
	| h w r |
	w := aForm width.
	h := aForm height.
	AuxForm
		ifNotNil: [
			w := w max: AuxForm width.
			h := h max: AuxForm height.
			(AuxForm width < w or: [ AuxForm height < h ])
				ifTrue: [ AuxForm := nil ]
				ifFalse: [ AuxForm bits atAllPut: 0 ]].
	AuxForm
		ifNil: [
			AuxForm := Form extent: w@h depth: 32.
			AuxBlitter := BitBlt toForm: AuxForm ].
	
	r := aForm boundingBox.
	AuxForm fill: r fillColor: aColor.
	AuxBlitter
		sourceForm: aForm;
		combinationRule: Form rgbMul;
		sourceRect: r;
		copyBits.
	^AuxForm
]

{ #category : 'cached state access' }
BitBltCanvas class >> cachedForms [
	CachedForms ifNil: [
		CachedForms := Dictionary new ].
	^CachedForms
]

{ #category : 'cached state access' }
BitBltCanvas class >> releaseClassCachedState [
	"
	BitBltCanvas releaseClassCachedState
	"
	AccessProtect := nil.
	AuxForm := nil.
	AuxBlitter := nil
]

{ #category : 'cached state access' }
BitBltCanvas class >> releaseClassState [
	"Do not remove them in every image save (#releaseClassCachedState) to save startup time."
	"
	BitBltCanvas releaseClassState
	"
	CachedForms := nil.
]

{ #category : 'cached forms' }
BitBltCanvas class >> topLeftCorner: r height: height [
	^self cachedForms
		at: { #topLeft. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				topLeftCorner: r
				height: height ]
]

{ #category : 'cached forms' }
BitBltCanvas class >> topRightCorner: r height: height [
	^self cachedForms
		at: { #topRight. r asInteger. height asInteger}
		ifAbsentPut: [
			Form
				topRightCorner: r
				height: height ]
]

{ #category : 'cached button forms' }
BitBltCanvas class >> windowButtonIcon: aSymbol size: finalSizeInteger [
	^self cachedForms
		at: { aSymbol . finalSizeInteger asInteger}
		ifAbsentPut: [
			| icon w h factor magnifiedExtent magnifiedIcon |
			icon := Theme current perform: aSymbol.	
			magnifiedIcon := icon.
			w := icon width.
			h := icon height.
			w*h = 0 ifFalse: [
				factor := 1.0 * finalSizeInteger / w min: 1.0 * finalSizeInteger / h.
				factor = 1.0 ifFalse: [
					magnifiedExtent := (icon extent * factor) rounded.
					magnifiedIcon := icon magnifyTo: magnifiedExtent ]].
			magnifiedIcon ]
]

{ #category : 'accessing' }
BitBltCanvas >> boundsFinderCanvas [
	^boundsFinderCanvas
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawCloseIcon [
	| icon |
	icon := self class windowButtonIcon: #closeIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawCollapseIcon [
	| icon |
	icon := self class windowButtonIcon: #collapseIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'morphic' }
BitBltCanvas >> drawCurrentAndSubmorphs [

	| currentNeedsDrawing submorphsNeedDrawing current |
	currentMorph visible ifFalse: [ ^self ].
	currentNeedsDrawing := true.
	submorphsNeedDrawing := currentMorph hasSubmorphs.
	clipRect notNil ifTrue: [
		self boundingRectOfCurrentIfTrivial ifNotNil: [ :mb |
			(mb intersects: clipRect) ifFalse: [
				currentNeedsDrawing := false.
				(currentMorph submorphsMightProtrude and: [currentMorph clipsSubmorphs not]) ifFalse: [
					submorphsNeedDrawing := false ]]]].

	currentNeedsDrawing ifTrue: [
		currentMorph drawOn: self.
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph ].

	"Hold currentMorph in a temp in case of breakage in recursive calls on submorphs."
	current := currentMorph.
	submorphsNeedDrawing ifTrue: [
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]]].

	currentNeedsDrawing ifTrue: [
		(current postDrawOn: self) ifTrue: [
			self displayBoundsUpdateOnCurrentMorph ].
		current isHighlighted ifTrue: [
			self drawHighlight: current ]].
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawDownIcon [
	| icon |
	icon := BitBltCanvas arrowOfDirection: #down size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawExpandIcon [
	| icon |
	icon := self class windowButtonIcon: #expandIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawExpandedAt: aPoint height: h [
	"For Tree View (IndentingListItemMorph)"

	| f |
	f := BitBltCanvas arrowOfDirection: #down size: h.
	self 
		image: f 
		at: aPoint x+(h//12) @ (aPoint y - (f height // 2)).
]

{ #category : 'morphic' }
BitBltCanvas >> drawHand [
	self 
		stencil: (Cursor cursorAt: #moveCursor)
		at: `-8 @ -8`
		color: `Color black`	.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawLeftIcon [
	| icon |
	icon := BitBltCanvas arrowOfDirection: #left size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawMenuIcon [
	| icon |
	icon := self class windowButtonIcon: #windowMenuIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawNotExpandedAt: aPoint height: h [
	"For Tree View (IndentingListItemMorph)"

	| f |
	f := BitBltCanvas arrowOfDirection: #right size: h.
	self 
		image: f 
		at: aPoint x+(h//12) @ (aPoint y - (f height // 2)).
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawPushPinIcon [
	| icon |
	icon := self class windowButtonIcon: #pushPinIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawRightIcon [
	| icon |
	icon := BitBltCanvas arrowOfDirection: #right size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-windows' }
BitBltCanvas >> drawRoundedFrame: aRectangle border: borderWidth color: borderColor topColor: titleAreaColor insideColor: insideColor labelHeight: labelHeight [

	self
		windowFrame: aRectangle
		color: borderColor
		radius: Theme current roundedWindowRadius
		border: borderWidth
		topColor: titleAreaColor
		labelHeight: labelHeight + borderWidth
		insideColor: insideColor
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawSquareCloseIcon [
	| icon |
	icon := self class windowButtonIcon: #squareCloseIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawSquareExpandIcon [
	| icon |
	icon := self class windowButtonIcon: #squareExpandIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawSquareMenuIcon [
	| icon |
	icon := self class windowButtonIcon: #squareMenuIcon size: currentMorph morphWidth.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-text' }
BitBltCanvas >> drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 := currentTransformation transform: aPoint roundedHAFZ.
	p1 := p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 := p1 roundedHAFZ.
	engine colorMap: nil.
	^aString asUnicodeString drawOnBitBltCanvasEngine: engine
		from: firstIndex
		to: lastIndex
		at: p1
		font: font
		color: aColor
]

{ #category : 'drawing-icons' }
BitBltCanvas >> drawUpIcon [
	| icon |
	icon := BitBltCanvas arrowOfDirection: #up size: Theme current scrollbarThickness.
	self pvtDrawButtonFaceForm: icon.
]

{ #category : 'drawing-ovals' }
BitBltCanvas >> ellipseCenter: center radius: radiusPointOrNumber borderWidth: mbw borderColor: mbc fillColor: morphFillColor [

	| displayRectangle doBorder doFill |
	doBorder := mbw > 0 and: [ mbc isTransparent not].
	doFill := morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle := (currentTransformation externalBoundingRectOf: (Rectangle center: center extent: radiusPointOrNumber asPoint * 2)) roundedHAFZ.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw roundedHAFZ].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw roundedHAFZ) ]]
]

{ #category : 'drawing-rectangles' }
BitBltCanvas >> fillRectangle: aRectangle color: aColor [
	"Fill the given rectangle."

	aColor isTransparent ifFalse: [
		self 
			frameAndFillRectangle: aRectangle
			fillColor: aColor
			borderWidth: 0
			borderColor: `Color transparent` ]
]

{ #category : 'drawing-rectangles' }
BitBltCanvas >> fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor [
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	(BitBltCanvas onForm: Display) fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.
	"
	self class accessProtect critical: [
		self class buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]
]

{ #category : 'private' }
BitBltCanvas >> fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule [

	| displayRectangle  additionalOffset clippedEngine targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex := patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle := (currentTransformation externalBoundingRectOf: aRectangle) roundedHAFZ.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset := `0@0`.
	clippedEngine := engine clippedBy: displayRectangle.
	targetTopLeft := clippedEngine clipRect topLeft truncateTo: ex.
	clipOffset := displayRectangle topLeft - targetTopLeft.
	additionalOffset := (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox := clippedEngine clipRect.
	savedMap := clippedEngine colorMap.
	clippedEngine sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedEngine destForm).
	top := (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left :=  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedEngine destOrigin: x@y; copyBits]].
	clippedEngine colorMap: savedMap
]

{ #category : 'drawing-rectangles' }
BitBltCanvas >> frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor [

	| rect bw |
	r hasPositiveExtent ifFalse: [^ self].
	rect := (currentTransformation externalBoundingRectOf: r) roundedHAFZ.
	bw := (currentTransformation externalizeScalar: borderWidth) roundedHAFZ.

	"draw the border of the rectangle"
	(bw = 0 or: [borderColor isTransparent]) ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]
]

{ #category : 'drawing-rectangles' }
BitBltCanvas >> frameRectangle: r borderWidth: borderWidth color: borderColor [
	"
	(BitBltCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |
	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect := (currentTransformation externalBoundingRectOf: r) roundedHAFZ.
		bw := (currentTransformation externalizeScalar: borderWidth) roundedHAFZ.
		self setPaintColor: borderColor.
		engine
			frameRect: rect
			borderWidth: bw ]
]

{ #category : 'private' }
BitBltCanvas >> frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor [
	"rect and borderWidth are in targetForm coordinates. No transformation is done."
	"
	(BitBltCanvas onForm: Display)
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		topLeftColor: Color green
		bottomRightColor: Color red.
	(BitBltCanvas onForm: Display) fillRectangle: (100@100 extent: 300@200) color: Color white.
	Display forceToScreen.
	"
	| w h |

	rect hasPositiveExtent ifFalse: [^ self].

	self setPaintColor: topLeftColor.

	engine frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w := h := borderWidth]
		ifFalse: [w := borderWidth x.   h := borderWidth y].
	self setPaintColor: bottomRightColor.
	engine 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h
]

{ #category : 'drawing-rectangles' }
BitBltCanvas >> frameRectangle: aRectangle topLeftColor: tlColor bottomRightColor: brColor borderWidth: borderWidth [
	"
	(BitBltCanvas onForm: Display)	
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| displayRectangle bw |
	bw := (currentTransformation externalizeScalar: borderWidth) roundedHAFZ.
	displayRectangle := (currentTransformation externalBoundingRectOf: aRectangle) roundedHAFZ.
	self
		frameRectangle: displayRectangle
		borderWidth: bw
		topLeftColor: tlColor
		bottomRightColor: brColor.
]

{ #category : 'drawing-images' }
BitBltCanvas >> image: aForm at: aPoint [
	"Draw a translucent image using the best available way of representing translucency."

	self image: aForm
		at: aPoint
		sourceRect: aForm boundingBox
]

{ #category : 'drawing-images' }
BitBltCanvas >> image: aForm at: aPoint sourceRect: sourceRect [
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."

	| r p |
	"p := (currentTransformation transform: aPoint) roundedHAFZ."
	p := (currentTransformation transform: (sourceRect translatedBy: aPoint) origin) roundedHAFZ.
	r := (form depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	engine colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	engine image: aForm at: p sourceRect: sourceRect rule: r.
	(form depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!)
		This is the same workaround as in #asFormOfDepth:"
		engine sourceForm: nil.
		engine combinationRule: 40. "fixAlpha:with:"
		engine copyBits ]
]

{ #category : 'drawing-images' }
BitBltCanvas >> image: aForm multipliedBy: aColor at: aPoint [
	"Multiply aForm and aColor, then blend over destination.
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel

	(BitBltCanvas onForm: Display) image: ((Form dotOfSize: 50) asFormOfDepth: 32) multipliedBy: Color red at: 20@20. Display forceToScreen
	"
	aColor isTransparent ifFalse: [
		self class accessProtect critical: [
			self class buildAuxWith: aForm multipliedWith: aColor.
			self image: AuxForm at: aPoint sourceRect: aForm boundingBox ]]
]

{ #category : 'initialization' }
BitBltCanvas >> initializeWithTranslation: aPoint [

	super initializeWithTranslation: aPoint.
	boundsFinderCanvas := BitBltBoundsFinderCanvas new initializeWithTranslation: aPoint.
]

{ #category : 'drawing' }
BitBltCanvas >> line: pt1 to: pt2 width: wp color: c [

	|  p1 p2 w |
	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 := (currentTransformation transform: pt1) roundedHAFZ.
		p2 := (currentTransformation transform: pt2) roundedHAFZ.
		w := (currentTransformation externalizeScalar: wp) roundedHAFZ.
		self setPaintColor: c.
		engine
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]
]

{ #category : 'private' }
BitBltCanvas >> nilState [
	"Nil possibly trouble making references. See senders."
	super nilState.
	boundsFinderCanvas notNil ifTrue: [
		boundsFinderCanvas nilState ].
]

{ #category : 'private-framework' }
BitBltCanvas >> privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor [
	"
	(BitBltCanvas onForm: Display)
		privateFrameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color green
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect := engine clipRect.
	engine clipRect: rect.
	engine
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
	engine clipRect: prevClipRect.
		
]

{ #category : 'private-framework' }
BitBltCanvas >> privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor [
	"
	(BitBltCanvas onForm: Display)
		privateFrameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect := engine clipRect.
	engine clipRect: rect.
	engine
		sourceForm: nil;
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.
	engine clipRect: prevClipRect.
]

{ #category : 'private-framework' }
BitBltCanvas >> privateFrameReverseGlobalRect: rect borderWidth: borderWidth [
	"
	(BitBltCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| prevClipRect |
	prevClipRect := engine clipRect.
	engine clipRect: rect.
	engine
		sourceForm: nil;
		fillColor: `Color gray: 0.5`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth.
	engine clipRect: prevClipRect.
]

{ #category : 'private' }
BitBltCanvas >> pvtDrawButtonFaceForm: aForm [
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	| color |
	color := currentMorph isPressed
		ifTrue: [ `Color gray: 0.75` ]
		ifFalse: [
			currentMorph mouseIsOver
				ifTrue: [ `Color gray: 0.75` ]
				ifFalse: [ `Color white` ]].
	self
		image: aForm
		multipliedBy: color
		at: (currentMorph morphExtent - aForm extent //2).
]

{ #category : 'morphic' }
BitBltCanvas >> restorePatch: savedPatch bounds: savedBounds [
	"Argument must previously be obtained by calling #savePatch:bounds:"

	self
		image: savedPatch
		at: savedBounds origin
		sourceRect: (0@0 extent: savedBounds extent)
]

{ #category : 'drawing-windows' }
BitBltCanvas >> roundBottomLeftCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor [
	"NOP here"
]

{ #category : 'drawing-windows' }
BitBltCanvas >> roundBottomRightCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor [
	"NOP here"
]

{ #category : 'drawing-windows' }
BitBltCanvas >> roundEdge: aRectangle border: borderWidth color: borderColor [
	"NOP here"
]

{ #category : 'drawing-roundedRectangles' }
BitBltCanvas >> roundRect: aRectangle color: aColor radius: aNumber [
	"
	(BitBltCanvas onForm: Display) roundRect: (10@10 extent: 200@100) color: Color red radius: 10.5. Display forceToScreen.
	"
	| r |
	r := (currentTransformation externalizeScalar: aNumber) roundedHAFZ.

	"top stripe"
	self
		image: (self class topLeftCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self fillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self fillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor
]

{ #category : 'drawing-windows' }
BitBltCanvas >> roundTopLeftCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor [
	"NOP here"
]

{ #category : 'drawing-windows' }
BitBltCanvas >> roundTopRightCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor [
	"NOP here"
]

{ #category : 'morphic' }
BitBltCanvas >> savePatch: prevSavedPatch bounds: aRectangle [
	"Senders don't need to know any details of what is answered, but just store it for further calls, and calls to restorePatch:"
	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."
	| savedPatch bb prevExtent extent |
	savedPatch := prevSavedPatch.
	prevExtent := 0@0.
	((savedPatch is: #Form) and: [
		prevExtent := savedPatch extent.
		prevExtent >= aRectangle extent]) 
			ifTrue: [
				savedPatch offset: 0@0 ]
			ifFalse: [
				"allocate new patch form if needed"
				extent := aRectangle extent max: prevExtent.
				extent := (extent x // 64 + 2 * 64) @ (extent y + 64).						"Make it slightly larger, and even width."
				savedPatch := Form extent: extent depth: form depth ].
	bb := BitBlt toForm: savedPatch.
	bb sourceForm: form; combinationRule: Form over;
		sourceX: aRectangle left; sourceY: aRectangle top;
		width: aRectangle width; height: aRectangle height;
		copyBits.
	^savedPatch
]

{ #category : 'private' }
BitBltCanvas >> setClipRect: aRectangle [
	"In targetForm coordinates"

	super setClipRect: aRectangle.
	engine clipRect: aRectangle.
	boundsFinderCanvas ifNotNil: [
		boundsFinderCanvas setClipRect: aRectangle ].
]

{ #category : 'private' }
BitBltCanvas >> setForm: aForm subPixelAntiAliasing: aBooleanOrNil [
	"nil means use default kind of anti aliasing. Ignored in BitBltCanvas."

	form := aForm.
	engine := BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0.
	self newClipRect: nil.
]

{ #category : 'private' }
BitBltCanvas >> setPaintColor: aColor [
	"Install a new color used for filling."
	| paintColor |
	paintColor := aColor ifNil: [ `Color transparent` ].
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
	engine combinationRule: Form blend
]

{ #category : 'drawing-images' }
BitBltCanvas >> stencil: stencilForm at: aPoint color: aColor [
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	"
	| dot |
	dot := ((Form dotOfSize: 32) asFormOfDepth: 1) offset: 0@0.
	dot displayAt: 20@20.
	(BitBltCanvas onForm: Display) stencil: dot at: 60@20 color: Color red.
	Display forceToScreen
	"
	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor
]

{ #category : 'drawing-images' }
BitBltCanvas >> stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor [
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p := (currentTransformation transform: aPoint) roundedHAFZ.
	self setPaintColor: aColor.
	engine colorMap: stencilForm maskingMap.
	engine stencil: stencilForm
		at: p
		sourceRect: sourceRect
]

{ #category : 'drawing-windows' }
BitBltCanvas >> windowFrame: aRectangle color: aColor radius: r border: bw topColor: titleAreaColor labelHeight: labelHeight insideColor: insideColor [

	| lh he tl tr |
	"top stripe"
	lh := labelHeight roundedHAFZ.
	self
		image: (self class topLeftCorner: r height: lh)
		multipliedBy: titleAreaColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: lh)
		multipliedBy: titleAreaColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		color: titleAreaColor.

	"left and right borders"
	tl := aRectangle topLeft + (0@lh).
	tr := aRectangle topRight + (bw negated@lh).
	he := bw@(aRectangle height - lh).
	self fillRectangle: (tl extent: he) color: aColor.
	self fillRectangle: (tr extent: he) color: aColor.
	
	"bottom stripe"
	self fillRectangle: ((aRectangle bottomLeft + (0@bw negated)) extent: (aRectangle width - 0@bw)) color: aColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor
]

{ #category : 'initialization' }
BitBltCanvas >> world: aWorldMorph [
	super world: aWorldMorph.
	boundsFinderCanvas world: aWorldMorph
]
