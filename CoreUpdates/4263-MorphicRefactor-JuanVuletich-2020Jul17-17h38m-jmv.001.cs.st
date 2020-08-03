'From Cuis 5.0 [latest update: #4259] on 17 July 2020 at 5:48:35 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/10/2020 16:20:44'!
boundingRectOfCurrentMorphAfterDraw
	"This rectangle is used for clipping submorphs in BitBltCanvas.
	In VectorCanvas we support clipping by any shape, not just rectangles. Then, this rectangle is used as an optimization of the area to be redrawn."
"
Y ademas es el rectangulo afectado por la ultima operacion de dibujado. Que se yo.
Renombrarlo onda #currentMorphDisplayBounds o #currentMorphAffectedRect o algo asi.
"
self flag: #puff.
	self subclassResponsibility! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/17/2020 17:41:13'!
boundingRectOfCurrentMorph
	"In targetForm coordinates"

	^currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds! !

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/10/2020 17:23:40'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates"
	self flag: #puff.
	^ self boundingRectOfCurrentMorph! !


!MorphicCanvas methodsFor: 'private' stamp: 'jmv 7/17/2020 17:45:55'!
clippingByCurrentMorphDo: aBlock
	| prevClipRect |

	prevClipRect _ self clipRect.
	"Might use the fact that currentMorph has just been drawn."
	self setClipRect: (prevClipRect intersect: self boundingRectOfCurrentMorphAfterDraw).
	self clippingMorph: currentMorph.
	aBlock ensure: [
		self clippingMorph: nil.
		self setClipRect: prevClipRect. ]! !


!BitBltCanvas methodsFor: 'testing' stamp: 'jmv 7/10/2020 17:23:43'!
isCurrentMorphVisible
	| aRectangle myClipRect |
	currentMorph visible ifFalse: [ ^false ].
	aRectangle := self boundingRectOfCurrentMorph.
	myClipRect := self clipRect.
	aRectangle right < myClipRect left	ifTrue: [^ false].
	aRectangle left > myClipRect right	ifTrue: [^ false].
	aRectangle bottom < myClipRect top	ifTrue: [^ false].
	aRectangle top > myClipRect bottom	ifTrue: [^ false].
	^ true
! !

!methodRemoval: BitBltCanvas #clippingRectForCurrentMorph stamp: 'jmv 7/17/2020 17:46:11'!
BitBltCanvas removeSelector: #clippingRectForCurrentMorph!
!methodRemoval: MorphicCanvas #clippingRectForCurrentMorph stamp: 'jmv 7/17/2020 17:46:11'!
MorphicCanvas removeSelector: #clippingRectForCurrentMorph!

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
('morphic' boundingRectOfCurrentMorphAfterDraw clippingMorph: displayBoundsInWorldOf: displayFullBoundsInWorldOf: drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline fullDraw: fullDrawHand: into: outOfMorph)
('other' contentsOfArea:into: showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isCurrentMorphVisible)
('private' canvasOrigin canvasToUse clippingByCurrentMorphDo: externalizeDisplayBounds:from: setClipRect: setForm:)
!

