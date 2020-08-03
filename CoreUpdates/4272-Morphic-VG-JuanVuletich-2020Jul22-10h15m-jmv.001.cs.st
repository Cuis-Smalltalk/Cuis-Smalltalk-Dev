'From Cuis 5.0 [latest update: #4268] on 22 July 2020 at 10:20:30 am'!

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/21/2020 11:21:47'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	engine ifNil: [ ^nil ].

^super textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc! !

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 7/21/2020 11:47:47'!
clippingByCurrentMorphDo: aBlock
"si clippeamos, tenemos un problemon.
queremos bounds no afectados por el clipping debido a los rectangles a repara
pero si por el owner. OJO!!
Creo que lo que habria que hacer es en #displayBounds:, si clippingMorph no es nil, pedirle sus displayBounds y hacer interseccion.
Y aca, ejecutar normalmente"
"	engine ifNil: [ ^self ]."
	^super clippingByCurrentMorphDo: aBlock! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 7/21/2020 11:25:28'!
line: pt1 to: pt2 width: wp color: c
	|  p1 p2 w |

	engine ifNil: [ ^nil ].

	(wp > 0 and: [ c isTransparent not ]) ifTrue: [
		p1 _ (currentTransformation transform: pt1) rounded.
		p2 _ (currentTransformation transform: pt2) rounded.
		w _ currentTransformation externalizeScalar: wp.
		self setPaintColor: c.
		engine
			width: w;
			height: w;
			drawFrom: p1 to: p2 ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 7/21/2020 11:24:36'!
image: aForm at: aPoint
	"Draw a translucent image using the best available way of representing translucency."


	engine ifNil: [ ^nil ].

	self image: aForm
		at: aPoint
		sourceRect: aForm boundingBox! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 7/21/2020 11:24:40'!
image: aForm at: aPoint sourceRect: sourceRect
	"Draw a translucent image using the best available way of representing translucency.
	Note: This will be fixed in the future."
	| r p |

	engine ifNil: [ ^nil ].

	p _ (currentTransformation transform: aPoint) rounded.
	r _ (self depth < 32 or: [ aForm mightBeTranslucent not ]) 
		ifTrue: [
			"Rule Form paint treats pixels with a value of zero as transparent"
			Form paint ]
		ifFalse: [ Form blend ].
	engine colorMap: (aForm colormapIfNeededFor: form); fillColor: nil.
	engine image: aForm at: p sourceRect: sourceRect rule: r.
	(self depth = 32 and: [ aForm depth < 32 ]) ifTrue: [
		"If we blit to 32bpp from one of smaller depth,
		it will have zero in the alpha channel (until BitBlt is fixed!!)
		This is the same workaround as in #asFormOfDepth:"
		engine sourceForm: nil.
		engine combinationRule: 40. "fixAlpha:with:"
		engine copyBits ]! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 7/21/2020 11:24:43'!
stencil: stencilForm at: aPoint color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels
	
	Display getCanvas stencil: (Form makeStar asFormOfDepth: 1) at: 20@20 color: Color red. Display forceToScreen
	"

	engine ifNil: [ ^nil ].

	^self stencil: stencilForm
		at: aPoint
		sourceRect: stencilForm boundingBox
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-images' stamp: 'jmv 7/21/2020 11:24:47'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	"Flood this canvas with aColor wherever stencilForm has non-zero pixels"
	| p |

	engine ifNil: [ ^nil ].

	p _ (currentTransformation transform: aPoint) rounded.
	self setPaintColor: aColor.
	engine colorMap: stencilForm maskingMap.
	engine stencil: stencilForm
		at: p
		sourceRect: sourceRect! !

!BitBltCanvas methodsFor: 'drawing-ovals' stamp: 'jmv 7/21/2020 11:24:27'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor

	| displayRectangle doBorder doFill |

	engine ifNil: [ ^nil ].

	doBorder _ mbw > 0 and: [ mbc isTransparent not].
	doFill _ morphFillColor isTransparent not.
	doBorder | doFill ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: (Rectangle center: mcx@mcy extent: mrx@mry * 2)) rounded.
		"draw the border of the oval"
		doBorder ifTrue: [
			self setPaintColor: mbc.
			engine frameOval: displayRectangle borderWidth: mbw].
		"fill the inside"
		doFill ifTrue: [
			self setPaintColor: morphFillColor.
			engine fillOval: (displayRectangle insetBy: mbw) ]]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/21/2020 11:23:50'!
fillRectangle: aRectangle color: aColor
	"Fill the given rectangle."


	engine ifNil: [ ^nil ].

	aColor isTransparent ifFalse: [
		self 
			frameAndFillRectangle: aRectangle
			fillColor: aColor
			borderWidth: 0
			borderColor: `Color transparent` ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/21/2020 11:23:54'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder


	engine ifNil: [ ^nil ].

	self fillRectangle: (aRectangle insetBy: borderWidth) color: aColor.
	self frameRectangle: aRectangle color: baseColorForBorder borderWidth: borderWidth borderStyleSymbol: aSymbol! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/21/2020 11:23:59'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (BitBltCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red. Display forceToScreen.
	"


	engine ifNil: [ ^nil ].

	self class accessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/21/2020 11:24:04'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	| rect bw |


	engine ifNil: [ ^nil ].

	r area = 0 ifTrue: [ ^self ].
	rect _ (currentTransformation externalizeRectangle: r) rounded.
	bw _ currentTransformation externalizeScalar: borderWidth.

	"draw the border of the rectangle"
	borderColor isTransparent ifFalse: [
		self setPaintColor: borderColor.
		engine frameRect: rect borderWidth: bw ].

	"fill the inside"
	fillColor isTransparent ifFalse: [
		self setPaintColor: fillColor.
		engine fillRect: (rect insetBy: bw) ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/21/2020 11:24:08'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	"
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	| rect bw |

	engine ifNil: [ ^nil ].

	(borderWidth > 0 and: [ borderColor isTransparent not ]) ifTrue: [
		rect _ (currentTransformation externalizeRectangle: r) rounded.
		bw _ currentTransformation externalizeScalar: borderWidth.
		self setPaintColor: borderColor.
		engine
			frameRect: rect
			borderWidth: bw ]! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/21/2020 11:24:17'!
frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white. Display forceToScreen.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 10
		borderStyleSymbol: #raised.
	Display forceToScreen.
	"

	| displayRectangle bw |

	engine ifNil: [ ^nil ].

	bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	aSymbol == #raised ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: r) rounded.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteWhiter
			bottomRightColor: aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		displayRectangle _ (currentTransformation externalizeRectangle: r) rounded.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteBlacker
			bottomRightColor: aColor quiteWhiter ].
	
	"Unrecognized border style. Draw some border..."
	self frameRectangle: r borderWidth: bw color: aColor! !

!BitBltCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 7/21/2020 11:24:21'!
reverseRectangleBorder: r borderWidth: borderWidth
	"
	Display getCanvas
		reverseRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	| rect |

	engine ifNil: [ ^nil ].

	rect _ (currentTransformation externalizeRectangle: r) rounded.
	engine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 7/21/2020 11:23:29'!
roundRect: aRectangle color: aColor radius: r
	"
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10
	"

	engine ifNil: [ ^nil ].

	"radious is not scaled properly..."
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

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 7/21/2020 11:23:34'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	BitBltCanvas releaseClassCachedState. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor |

	engine ifNil: [ ^nil ].

	"top stripe"
	self
		image: (self class topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) color: bottomColor! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 7/21/2020 11:23:38'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	"
	Display restore.
	BitBltCanvas releaseClassCachedState. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35
	"
	| h2 |

	engine ifNil: [ ^nil ].

	"top stripe"
	self
		image: (self class topLeftCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: h1) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor.
	
	"bottom stripe"
	h2 _ aRectangle height - h1.
	self
		image: (self class bottomLeftCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft + (0@h1).
	self
		image: (self class bottomRightCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight + (r negated@h1).
	self
		fillRectangle: ((aRectangle topLeft + (r@h1)) extent: (aRectangle width-r-r@h2))
		tilingWith: (self class verticalGrayGradient: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.! !

!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 7/21/2020 11:23:43'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	BitBltCanvas releaseClassCachedState.
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	Display forceToScreen
	"
	"top stripe"
	| bottomColor he tl tr |

	engine ifNil: [ ^nil ].

	self
		image: (self class topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ aRectangle topLeft + (0@lh).
	tr _ aRectangle topRight + (bw negated@lh).
	he _ bw@(aRectangle height - lh - r).
	self fillRectangle: (tl extent: he) color: bottomColor.
	self fillRectangle: (tr extent: he) color: bottomColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1 borderWidth: bw)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/21/2020 11:18:45'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"

	| p1 font |
	engine ifNil: [ ^nil ].

	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ fontOrNil ifNil: [ FontFamily defaultFamilyAndPointSize ].
	^font
		onBitBltCanvasEngine: engine
		displayString: aString
		from: firstIndex
		to: lastIndex
		at: p1
		color: aColor! !

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 7/21/2020 11:22:10'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	"Answer last affected pixel position
	Answer nil if nothing was done"
	| p1 font |

	engine ifNil: [ ^nil ].

	p1 _ (currentTransformation transform: aPoint rounded) rounded.
	engine colorMap: nil.
	font _ fontOrNil ifNil: [ FontFamily defaultFamilyAndPointSize ].

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

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 7/21/2020 11:28:05'!
setClipRect: aRectangle
	"In targetForm coordinates"

	super setClipRect: aRectangle.

	engine ifNil: [ ^nil ].

	engine clipRect: aRectangle! !

