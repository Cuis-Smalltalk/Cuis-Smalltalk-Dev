'From Cuis 5.0 [latest update: #4712] on 29 July 2021 at 12:26:42 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/29/2021 12:20:08'!
displayBoundsForHand: aHand

	self subclassResponsibility.! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/29/2021 12:21:09'!
displayBoundsForHand: aHand

	^ aHand morphPosition-8 extent: aHand morphExtent.! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/29/2021 12:22:05'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch. If necessary, handle the transition to using the hardware cursor."

	self needsRedraw: false.
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		submorphs notEmpty ifTrue: [ ^ self ].
		"Make the transition to using hardware cursor. Clear savedPatch and
		 report one final damage rectangle to erase the image of the software cursor."
		"		owner invalidateDisplayRect: (savedPatch offset extent: savedPatch extent) from: nil."
		self invalidateDisplayRect: (aCanvas displayBoundsForHand: self) for: nil.
		Cursor currentCursor == Cursor defaultCursor ifFalse: [ Cursor defaultCursor activateCursor ].
		"show hardware cursor"
		prevFullBounds _ nil ].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/29/2021 12:18:09'!
drawHand
	"For the user Hand. Especially when carrying morphs around."

	self subclassResponsibility.! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/29/2021 12:18:20'!
drawHand
	self 
		stencil: (Cursor cursorAt: #moveCursor)
		at: `-8 @ -8`
		color: `Color black`	.! !


!MorphicCanvas reorganize!
('accessing' clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawLeftIcon drawMenuIcon drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-widgets' drawButtonIconFromCurrentMorph)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('initialization' initializeWith:origin:preferSubPixelAntiAliasing: world:)
('morphic' canDraw: clipCurrentMorph: currentMorphDrawingFails displayBoundsForHand: drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline drawHand drawHighlight: fullDraw: fullDrawHand: into: outOfMorph restorePatch:bounds: savePatch:bounds:)
('other' showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isCurrentMorphVisible)
('private' boundingRectOfCurrentMorphAfterDraw boundingRectOfCurrentMorphFromLocalBounds canvasOrigin canvasToUse clippingByCurrentMorphDo: externalizeDisplayBounds:from: fontToUse: setClipRect: setForm:preferSubPixelAntiAliasing: updatingMorphBoundsDo:)
('morphic world' computeDamage:repair:rootMorphs:rootMorphsDamage: drawWorld:repair: drawWorld:rootMorphs:rootMorphsDamage:allDamage: drawWorldBackground:rects: fullAddCurrentRect:submorphs:to: fullAddRedrawRect:to: fullUpdateBounds: fullUpdateCurrentBounds updateHandsDisplayBounds:)
!


!BitBltCanvas reorganize!
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:topLeftColor:bottomRightColor:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:from:to:atBaseline:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawLeftIcon drawMenuIcon drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight: roundBottomLeftCornerX:y:length:border:color: roundBottomRightCornerX:y:length:border:color: roundEdge:color: roundTopLeftCornerX:y:length:border:color: roundTopRightCornerX:y:length:border:color: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: fontToUse: frameRectangle:borderWidth:topLeftColor:bottomRightColor: pvtDrawButtonFaceForm: setClipRect: setForm:preferSubPixelAntiAliasing: setPaintColor: updatingMorphBoundsDo:)
('morphic' boundingRectOfCurrentMorphAfterDraw canDraw: displayBoundsForHand: drawCurrentAndSubmorphs drawHand morph:isAtPoint:)
!

