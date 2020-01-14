'From Cuis 5.0 [latest update: #3959] on 11 January 2020 at 5:38:50 pm'!

!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 1/11/2020 17:37:21'!
line: pt1 to: pt2 width: wp color: c
	|  p1 p2 w |
	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 _ (currentTransformation transform: pt1) rounded.
		p2 _ (currentTransformation transform: pt2) rounded.
		w _ currentTransformation externalizeScalar: wp.
		self setPaintColor: c.
		port ifNil: [ self resetGrafPort ].
		port
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 1/11/2020 17:38:10'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |
	p _ (currentTransformation transform: aPoint) rounded.
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
		port ifNil: [ self resetGrafPort ].
	port colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	port image: aForm at: p sourceRect: sourceRect rule: r.
	(self depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!!)
		This is the same workaround as in #asFormOfDepth:"
		port sourceForm: nil.
		port combinationRule: 40. "fixAlpha:with:"
		port copyBits ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 1/11/2020 17:38:02'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |
	p _ (currentTransformation transform: aPoint) rounded.
	self setPaintColor: aColor.
		port ifNil: [ self resetGrafPort ].
	port colorMap: stencilForm maskingMap.
	port stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 1/11/2020 17:38:14'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |
	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) rounded.
		"draw the border of the oval"
		port ifNil: [ self resetGrafPort ].
		doBorder ifTrue: [
			self setPaintColor: mbc.
			port frameOval: displayRectangle borderWidth: mbw].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			port fillOval: (displayRectangle insetBy: mbw) ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 1/11/2020 17:38:07'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect bw |

	r area = 0 ifTrue: [ ^self ].
	rect _ (currentTransformation externalizeRectangle: r) rounded.
	bw _ currentTransformation externalizeScalar: borderWidth.

	"draw the border of the rectangle"
		port ifNil: [ self resetGrafPort ].
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		port frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		port fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 1/11/2020 17:37:34'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |
	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect _ (currentTransformation externalizeRectangle: r) rounded.
		bw _ currentTransformation externalizeScalar: borderWidth.
		self setPaintColor: borderColor.
		port ifNil: [ self resetGrafPort ].
		port
			frameRect: rect
			borderWidth: bw ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 1/11/2020 17:37:55'!
reverseRectangleBorder: r borderWidth: borderWidth
	"
	Display getCanvas
		reverseRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| rect |
	rect _ (currentTransformation externalizeRectangle: r) rounded.
		port ifNil: [ self resetGrafPort ].
	port
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 1/11/2020 17:38:18'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint) rounded.
		port ifNil: [ self resetGrafPort ].
	port colorMap: nil.
	font _ fontOrNil ifNil: [ AbstractFont default ].
	^font on: port
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 17:37:51'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule

	| displayRectangle  additionalOffset clippedPort targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ (currentTransformation externalizeRectangle: aRectangle) rounded.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	additionalOffset _ `0@0`.
		port ifNil: [ self resetGrafPort ].
	clippedPort _ port clippedBy: displayRectangle.
	targetTopLeft _ clippedPort clipRect topLeft truncateTo: ex.
	clipOffset _ displayRectangle topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedPort clipRect.
	savedMap _ clippedPort colorMap.
	clippedPort sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedPort destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedPort destOrigin: x@y; copyBits]].
	clippedPort colorMap: savedMap! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 17:37:40'!
frameRectangle: rect borderWidth: borderWidth topLeftColor: topLeftColor bottomRightColor: bottomRightColor
	"
	rect and borderWidth are in form coordinates. No transformation is done.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		topLeftColor: Color green
		bottomRightColor: Color red
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white
	"
	| w h |

	rect area = 0 ifTrue: [^ self].

	self setPaintColor: topLeftColor.

		port ifNil: [ self resetGrafPort ].
	port frameRectTopLeft: rect borderWidth: borderWidth.

	borderWidth isNumber
		ifTrue: [w _ h _ borderWidth]
		ifFalse: [w _ borderWidth x.   h _ borderWidth y].
	self setPaintColor: bottomRightColor.
	port 
		 frameRectRight: rect width: w;
		 frameRectBottom: rect height: h! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 17:37:44'!
setClipRect: aRectangle
	"In form coordinates"

	super setClipRect: aRectangle.
		port ifNil: [ self resetGrafPort ].
	port clipRect: aRectangle! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 17:37:59'!
setPaintColor: aColor
	"Install a new color used for filling."
	| paintColor |
	paintColor _ aColor ifNil: [ `Color transparent` ].
	(paintColor is: #Color) ifFalse: [
		^ self error: 'Cannot install color' ].

	"Okay, so paintColor really *is* a color"
		port ifNil: [ self resetGrafPort ].
	port sourceForm: nil.
	(paintColor isOpaque or: [ self depth < 32]) ifTrue: [
		port fillColor: paintColor.
		port combinationRule: Form paint.
		^ self ].

	"BitBlt setup for alpha mapped transfer"
	port fillColor: paintColor.
	port combinationRule: Form blend! !

