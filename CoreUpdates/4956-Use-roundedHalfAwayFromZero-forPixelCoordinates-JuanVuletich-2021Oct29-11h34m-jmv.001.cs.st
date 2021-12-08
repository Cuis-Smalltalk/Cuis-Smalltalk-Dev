'From Cuis 5.0 [latest update: #4955] on 29 October 2021 at 11:36:43 am'!

!Number methodsFor: 'truncation and round off' stamp: 'jmv 10/29/2021 09:53:30'!
rounded
	"Answer the integer nearest the receiver."
	"See https://en.wikipedia.org/wiki/Rounding#Round_half_to_even"

	| truncated fractionPartAbs |
	truncated _ self truncated.
	fractionPartAbs _ (self-truncated) abs.
	fractionPartAbs = `1/2`
		ifTrue: [ truncated even ifTrue: [^truncated] ifFalse: [^truncated + self sign]].
	fractionPartAbs < `1/2`
		ifTrue: [^ truncated]
		ifFalse: [^ truncated + self sign]! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/29/2021 10:52:28'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	p1 _ pt roundedHAFZ.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 roundedHAFZ.
	displayEngine colorMap: nil.
	^font
		onBitBltCanvasEngine: displayEngine
		displayString: s
		from: 1
		to: s size
		at: p1
		color: aColor! !


!BitBlt methodsFor: 'line drawing' stamp: 'jmv 10/29/2021 10:52:36'!
drawFrom: startPoint to: stopPoint withFirstPoint: drawFirstPoint
	"Draw a line whose end points are startPoint and stopPoint.
	The line is formed by repeatedly calling copyBits at every
	point along the line.  If drawFirstPoint is false, then omit
	the first point so as not to overstrike at line junctions."
	| offset point1 point2 forwards |
	"Always draw down, or at least left-to-right"
	forwards _ (startPoint y = stopPoint y and: [startPoint x < stopPoint x])
				or: [startPoint y < stopPoint y].
	forwards
		ifTrue: [point1 _ startPoint. point2 _ stopPoint]
		ifFalse: [point1 _ stopPoint. point2 _ startPoint].
	sourceForm
		ifNil: [
			destX := point1 x.
			destY := point1 y]
		ifNotNil: [
			width := sourceForm width.
			height := sourceForm height.
			offset := sourceForm offset.
			destX := (point1 x + offset x) roundedHAFZ.
			destY := (point1 y + offset y) roundedHAFZ].

	"Note that if not forwards, then the first point is the last and vice versa.
	We agree to always paint stopPoint, and to optionally paint startPoint."
	(drawFirstPoint or: [forwards == false  "ie this is stopPoint"])
		ifTrue: [self copyBits].
	self drawLoopX: (point2 x - point1 x) roundedHAFZ 
				  Y: (point2 y - point1 y) roundedHAFZ.
	(drawFirstPoint or: [forwards  "ie this is stopPoint"])
		ifTrue: [self copyBits].
! !


!WarpBlt class methodsFor: 'form rotation' stamp: 'jmv 10/29/2021 10:52:41'!
rotate: srcForm degrees: angleInDegrees center: aPoint scaleBy: scalePoint smoothing: cellSize
	"Rotate the given Form the given number of degrees about the given center and scale its width and height by x and y of the given scale point. Smooth using the given cell size, an integer between 1 and 3, where 1 means no smoothing. Return a pair where the first element is the rotated Form and the second is the position offset required to align the center of the rotated Form with that of the original. Note that the dimensions of the resulting Form generally differ from those of the original."

	| srcRect center radians dstOrigin dstCorner p dstRect inverseScale quad dstForm newCenter warpSrc |
	srcRect _ srcForm boundingBox.
	center _ srcRect center.
	radians _ angleInDegrees degreesToRadians.
	dstOrigin _ dstCorner _ center.
	srcRect corners do: [:corner |
		"find the limits of a rectangle that just encloses the rotated
		 original; in general, this rectangle will be larger than the
		 original (e.g., consider a square rotated by 45 degrees)"
		p _ ((corner - center) scaledBy: scalePoint) + center.
		p _ (p inverseRotatedBy: radians about: center) roundedHAFZ.
		dstOrigin _ dstOrigin min: p.
		dstCorner _ dstCorner max: p].

	"rotate the enclosing rectangle back to get the source quadrilateral"
	dstRect _ dstOrigin corner: dstCorner.
	inverseScale _ (1.0 / scalePoint x)@(1.0 / scalePoint y).
	quad _ dstRect innerCorners collect: [:corner |
		p _ corner inverseRotatedBy: radians negated about: center.
		((p - center) scaledBy: inverseScale) + center].

	"make a Form to hold the result and do the rotation"
	warpSrc _ srcForm.
	(srcForm is: #ColorForm)
		ifTrue: [
			cellSize > 1 | true "ar 12/27/2001: Always enable - else sketches won't work"
				ifTrue: [
					warpSrc _ Form extent: srcForm extent depth: 16.
					srcForm displayOn: warpSrc.
					dstForm _ Form extent: dstRect extent depth: 16]  "use 16-bit depth to allow smoothing"
				ifFalse: [
					dstForm _ srcForm class extent: dstRect extent depth: srcForm depth]]
		ifFalse: [
			dstForm _ srcForm class extent: dstRect extent depth: srcForm depth].

	(WarpBlt toForm: dstForm)
		sourceForm: warpSrc;
		colorMap: (warpSrc colormapIfNeededFor: dstForm);
		cellSize: cellSize;  "installs a new colormap if cellSize > 1"
		combinationRule: Form paint;
		copyQuad: quad toRect: dstForm boundingBox.

	(dstForm is: #ColorForm) ifTrue: [ dstForm colors: srcForm colors copy ].
	newCenter _ (center inverseRotatedBy: radians about: aPoint) truncated.
	^ Array with: dstForm with: dstRect origin + (newCenter - center)
! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/29/2021 10:53:23'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			anchoredFormOrMorph morphPosition:
				(destX@ (lineY+line baseline)) + (anchoredFormOrMorph morphPosition-anchoredFormOrMorph fullBoundsInOwner corner) roundedHAFZ.
			anchoredFormOrMorph show.
			canvas fullDraw: anchoredFormOrMorph ]
		ifFalse: [
			destY _ lineY.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true! !


!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 10/29/2021 10:53:27'!
drawOn: aCanvas

	| x colorToUse centeringOffset |
	isHighlighted ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: ((Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) alpha: 0.3) ].

	isSelected ifTrue: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].

	x _ 12 * indentLevel.

	complexContents hasContents ifTrue: [
		isExpanded
			ifTrue: [ aCanvas drawExpandedAt: x@(extent y//2)]
			ifFalse: [ aCanvas drawNotExpandedAt: x@(extent y//2) ]].
	x _ x + 18.

	icon isNil ifFalse: [
		centeringOffset _ ((extent y - icon height) / 2.0) roundedHAFZ.
		 aCanvas 
			image: icon
			at:  (x @ centeringOffset).
		x _ x + 20 ].

	colorToUse _ complexContents preferredColor ifNil: [ color ].
	aCanvas
		drawString: contents asString
		at: x@0
		font: self fontToUse
		color: colorToUse! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 10/29/2021 10:54:26'!
line: pt1 to: pt2 width: wp color: c

	|  p1 p2 w |
	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 _ (currentTransformation transform: pt1) roundedHAFZ.
		p2 _ (currentTransformation transform: pt2) roundedHAFZ.
		w _ (currentTransformation externalizeScalar: wp) roundedHAFZ.
		self setPaintColor: c.
		engine
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 10/29/2021 10:54:12'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."

	| r p |
	p _ (currentTransformation transform: aPoint) roundedHAFZ.
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

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 10/29/2021 10:54:30'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ (currentTransformation transform: aPoint) roundedHAFZ.
	self setPaintColor: aColor.
	engine colorMap: stencilForm maskingMap.
	engine stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 10/29/2021 10:53:55'!
ellipseCenter: center radius: radiusPointOrNumber borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |
	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalBoundingRectOf: (Rectangle center: center extent: radiusPointOrNumber asPoint * 2)) roundedHAFZ.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw roundedHAFZ].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw roundedHAFZ) ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/29/2021 10:54:03'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor

	| rect bw |
	r area = 0 ifTrue: [ ^self ].
	rect _ (currentTransformation externalBoundingRectOf: r) roundedHAFZ.
	bw _ (currentTransformation externalizeScalar: borderWidth) roundedHAFZ.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/29/2021 10:54:06'!
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
		rect _ (currentTransformation externalBoundingRectOf: r) roundedHAFZ.
		bw _ (currentTransformation externalizeScalar: borderWidth) roundedHAFZ.
		self setPaintColor: borderColor.
		engine
			frameRect: rect
			borderWidth: bw ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 10/29/2021 10:54:09'!
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
	bw _ (currentTransformation externalizeScalar: borderWidth) roundedHAFZ.
	displayRectangle _ (currentTransformation externalBoundingRectOf: aRectangle) roundedHAFZ.
	self
		frameRectangle: displayRectangle
		borderWidth: bw
		topLeftColor: tlColor
		bottomRightColor: brColor.! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 10/29/2021 10:54:28'!
roundRect: aRectangle color: aColor radius: aNumber
	"
	(BitBltCanvas onForm: Display) roundRect: (10@10 extent: 200@100) color: Color red radius: 10.5. Display forceToScreen.
	"
	| r |
	r _ (currentTransformation externalizeScalar: aNumber) roundedHAFZ.

	"top stripe"
	self
		image: (self class topLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self fillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self fillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/29/2021 10:53:34'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ currentTransformation transform: aPoint roundedHAFZ.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 roundedHAFZ.
	engine colorMap: nil.
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/29/2021 10:53:40'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"
	| p1 |
	p1 _ (currentTransformation transform: aPoint roundedHAFZ) roundedHAFZ.
	engine colorMap: nil.

	aColor = `Color black` ifFalse: [ | topColor |
		topColor _ aColor alphaMixed: 0.25 with: `Color black`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1
			color: topColor ].
	aColor = `Color white` ifFalse: [ | bottomColor |
		bottomColor _ aColor alphaMixed: 0.22 with: `Color white`.
		font
			onBitBltCanvasEngine: engine
			displayString: aString
			from: firstIndex
			to: lastIndex
			at: p1 + `0@2`
			color: bottomColor ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1 + `0@1`
		color: aColor.! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 10/29/2021 10:53:58'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule

	| displayRectangle  additionalOffset clippedEngine targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ (currentTransformation externalBoundingRectOf: aRectangle) roundedHAFZ.

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

