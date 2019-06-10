'From Cuis 5.0 of 7 November 2016 [latest update: #3777] on 9 June 2019 at 10:04:40 pm'!

!MorphicCanvas methodsFor: 'drawing' stamp: 'pb 5/31/2019 19:35:57'!
line: pt1 to: pt2 width: wp color: c
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'pb 5/31/2019 19:45:10'!
drawButtonIconFromCurrentMorph
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-images' stamp: 'pb 5/31/2019 19:39:09'!
image: aForm at: aPoint
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-images' stamp: 'pb 5/31/2019 19:39:29'!
image: aForm at: aPoint sourceRect: sourceRect
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-images' stamp: 'pb 5/31/2019 19:39:52'!
image: aForm multipliedBy: aColor at: aPoint
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-images' stamp: 'pb 5/31/2019 19:40:07'!
stencil: stencilForm at: aPoint color: aColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-images' stamp: 'pb 5/31/2019 19:40:17'!
stencil: stencilForm at: aPoint sourceRect: sourceRect color: aColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-ovals' stamp: 'pb 5/31/2019 19:34:51'!
ellipseCenterX: mcx y: mcy rx: mrx ry: mry borderWidth: mbw borderColor: mbc fillColor: morphFillColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 5/31/2019 19:41:12'!
fillRectangle: aRectangle color: aColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 5/31/2019 19:41:22'!
fillRectangle: aRectangle color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol baseColorForBorder: baseColorForBorder
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 5/31/2019 19:41:31'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 5/31/2019 19:41:47'!
frameAndFillRectangle: r fillColor: fillColor borderWidth: borderWidth borderColor: borderColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 5/31/2019 19:42:05'!
frameRectangle: r borderWidth: borderWidth color: borderColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 5/31/2019 19:42:23'!
frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-rectangles' stamp: 'pb 5/31/2019 19:42:33'!
reverseRectangleBorder: r borderWidth: borderWidth
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'pb 5/31/2019 19:43:02'!
roundRect: aRectangle color: aColor radius: r
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'pb 5/31/2019 19:43:12'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'pb 5/31/2019 19:43:22'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'pb 5/31/2019 19:43:31'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'pb 5/31/2019 19:38:04'!
drawString: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor
	self subclassResponsibility ! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'pb 5/31/2019 19:49:08'!
clippingRectForCurrentMorph
	"In form coordinates"
	"En M3, es el obtenido al dibujarlo, recien.
Dejar eso claro en el nombre. Eliminar 'clipping'"
	^ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.! !

!MorphicCanvas methodsFor: 'testing' stamp: 'pb 5/31/2019 19:50:24'!
isCurrentMorphVisible
	| aRectangle |
	currentMorph visible ifFalse: [ ^ false ].
	aRectangle _ currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds.
	"Optimization"
	aRectangle right < clipLeft ifTrue: [ ^ false ].
	aRectangle left > (clipRight + 1) ifTrue: [ ^ false ].
	aRectangle bottom < clipTop ifTrue: [ ^ false ].
	aRectangle top > (clipBottom + 1) ifTrue: [ ^ false ].
	^ true.! !

!MorphicCanvas methodsFor: 'private' stamp: 'pb 5/31/2019 19:32:16'!
setForm: aForm
	form _ aForm.! !


!BitBltCanvas methodsFor: 'private' stamp: 'pb 5/31/2019 19:32:31'!
setForm: aForm
	super setForm: aForm.
	self resetGrafPort.
	self newClipRect: nil.! !

!methodRemoval: BitBltCanvas #clippingRectForCurrentMorph!
BitBltCanvas removeSelector: #clippingRectForCurrentMorph!
!methodRemoval: BitBltCanvas #isCurrentMorphVisible!
BitBltCanvas removeSelector: #isCurrentMorphVisible!

!MorphicCanvas reorganize!
('accessing' clipRect currentTransformation depth extent form newClipRect:)
('drawing' line:to:width:color:)
('drawing-icons' drawButtonIconFromCurrentMorph)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:from:to:at:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('initialization' initializeWith:origin:)
('morphic' clippingRectForCurrentMorph currentOwnerIfClips: displayBoundsInWorldOf: displayFullBoundsInWorldOf: drawCurrentAsError drawCurrentAsOutline fullDraw: fullDrawHand: into: outOfMorph)
('other' contentsOfArea:into: showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isCurrentMorphVisible)
('private' canvasOrigin externalizeDisplayBounds:from: setClipRect: setForm:)
!


!BitBltCanvas reorganize!
('drawing' line:to:width:color:)
('drawing-icons' drawButtonIconFromCurrentMorph)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:from:to:at:font:color:)
('initialization' initializeWith:origin:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: frameRectangle:borderWidth:topLeftColor:bottomRightColor: resetGrafPort setClipRect: setForm: setPaintColor:)
!

