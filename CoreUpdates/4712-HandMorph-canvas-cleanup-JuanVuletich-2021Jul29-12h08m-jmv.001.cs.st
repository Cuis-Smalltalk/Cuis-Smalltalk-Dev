'From Cuis 5.0 [latest update: #4711] on 29 July 2021 at 12:09:37 pm'!

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:34'!
drawCloseIcon
	"For Window control buttons"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:37'!
drawCollapseIcon
	"For Window control buttons"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:17'!
drawDownIcon
	"For Scrollbars"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:42'!
drawExpandIcon
	"For Window control buttons"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:37:03'!
drawHand
	"For the user Hand. Especially when carrying morphs around."

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:14'!
drawLeftIcon
	"For Scrollbars"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:46'!
drawMenuIcon
	"For Window control buttons"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:33:11'!
drawPushPinIcon
	"For Menu stay-up button"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:10'!
drawRightIcon
	"For Scrollbars"

	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:32:05'!
drawUpIcon
	"For Scrollbars"

	self subclassResponsibility.! !


!BitBltCanvas methodsFor: 'drawing-icons' stamp: 'jmv 7/29/2021 10:36:51'!
drawHand
	self 
		stencil: (Cursor cursorAt: #moveCursor)
		at: `-8 @ -8`
		color: `Color black`	.! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/29/2021 10:37:10'!
drawOn: aCanvas
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."

	aCanvas drawHand! !


!MorphicCanvas reorganize!
('accessing' clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawHand drawLeftIcon drawMenuIcon drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-widgets' drawButtonIconFromCurrentMorph)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('initialization' initializeWith:origin:preferSubPixelAntiAliasing: world:)
('morphic' canDraw: clipCurrentMorph: currentMorphDrawingFails drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline drawHighlight: fullDraw: fullDrawHand: into: outOfMorph restorePatch:bounds: savePatch:bounds:)
('other' showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isCurrentMorphVisible)
('private' boundingRectOfCurrentMorphAfterDraw boundingRectOfCurrentMorphFromLocalBounds canvasOrigin canvasToUse clippingByCurrentMorphDo: externalizeDisplayBounds:from: fontToUse: setClipRect: setForm:preferSubPixelAntiAliasing: updatingMorphBoundsDo:)
('morphic world' computeDamage:repair:rootMorphs:rootMorphsDamage: drawWorld:repair: drawWorld:rootMorphs:rootMorphsDamage:allDamage: drawWorldBackground:rects: fullAddCurrentRect:submorphs:to: fullAddRedrawRect:to: fullUpdateBounds: fullUpdateCurrentBounds updateHandsDisplayBounds:)
!

