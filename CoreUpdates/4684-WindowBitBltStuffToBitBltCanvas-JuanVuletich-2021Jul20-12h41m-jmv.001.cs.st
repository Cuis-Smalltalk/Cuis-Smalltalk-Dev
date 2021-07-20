'From Cuis 5.0 [latest update: #4683] on 20 July 2021 at 12:50:59 pm'!

!SystemWindow methodsFor: 'panes' stamp: 'jmv 7/20/2021 11:17:26'!
windowFrameColor

	| windowFrameColor |
	windowFrameColor _ self borderColor.
	self isTopWindow
		ifTrue: [ windowFrameColor _ windowFrameColor lighter ].
	^windowFrameColor! !


!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/20/2021 10:24:23'!
drawButtonRound: aPluggableButtonMorph
	| r colorForButton rect color |
	color _ aPluggableButtonMorph color.
	colorForButton _ aPluggableButtonMorph isPressed
		ifFalse: [
			aPluggableButtonMorph mouseIsOver
				ifTrue: [	 Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [ color ]]
		ifTrue: [ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		rect _ aPluggableButtonMorph morphLocalBounds insetBy: `1@3`.
		self roundRect: rect color: colorForButton radius: r ].

	Theme current embossedButtonLabels
		ifTrue: [ self drawButtonEmbossedLabel: aPluggableButtonMorph ]
		ifFalse: [ self drawButtonRegularLabel: aPluggableButtonMorph ].! !

!MorphicCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 09:40:57'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor insideColor: insideColor labelHeight: labelHeight

	self subclassResponsibility.! !


!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 09:40:38'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor insideColor: insideColor labelHeight: labelHeight
	"Title area is not inside window borders"
	| bottomFactor topFactor |
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor _ Theme current titleGradientTopFactor.
			bottomFactor _ Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor _ 1.
			bottomFactor _ 1 ].
	self
		windowFrame: aRectangle
		color: borderColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: insideColor! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 10:57:33'!
roundBottomLeftCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor
	"NOP here"! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 10:57:40'!
roundBottomRightCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor
	"NOP here"! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 11:22:31'!
roundEdge: aRectangle color: aColor
	"NOP here"! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 10:45:30'!
roundTopLeftCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor
	"NOP here"! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 10:51:07'!
roundTopRightCornerX: cornerX y: cornerY length: l border: borderWidth color: borderColor
	"NOP here"! !


!WindowEdgeAdjustingMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2021 11:23:14'!
drawOn: aCanvas

	| c |
	c _ owner windowFrameColor.
	selector caseOf: {
		[ #windowTopLeft: ] -> [
			aCanvas roundTopLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c.
			].
		[ #windowTopRight: ] -> [
			aCanvas roundTopRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c.
			].
		[ #windowBottomLeft: ] -> [
			aCanvas roundBottomLeftCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c.
			].
		[ #windowBottomRight: ] -> [
			aCanvas roundBottomRightCornerX: 0 y: 0 length: extent x border: owner borderWidth color: c.
			].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."	
		aCanvas roundEdge: self morphLocalBounds color: c ].! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 7/20/2021 11:18:36'!
drawClassicFrameOn: aCanvas color: windowFrameColor
	"Window border encompasses title area. No round corners. No title gradient."

	aCanvas fillRectangle: self morphLocalBounds color: color borderWidth: borderWidth borderStyleSymbol: #simple baseColorForBorder: self widgetsColor.

	"A border was drawn at the left, top and right of the title area.
	The look is that the title area is inside the window"
	aCanvas fillRectangle: (borderWidth@borderWidth extent: extent x - (2*borderWidth)@ self labelHeight) color: windowFrameColor! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 7/20/2021 11:24:42'!
drawOn: aCanvas
	| windowFrameColor roundCorners |
	windowFrameColor _ self windowFrameColor.
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			"Round corners. Optional title gradient."
			self drawRoundedFrameOn: aCanvas color: windowFrameColor ]
		ifFalse: [
			"No round corners. No title gradient."
			self drawClassicFrameOn: aCanvas color: windowFrameColor ].
	labelString ifNotNil: [ self drawLabelOn: aCanvas ]! !

!SystemWindow methodsFor: 'drawing' stamp: 'jmv 7/20/2021 09:50:55'!
drawRoundedFrameOn: aCanvas color: widgetsColor

	aCanvas
		drawRoundedFrame: self morphLocalBounds
		border: borderWidth 
		color: widgetsColor
		insideColor: color
		labelHeight: self labelHeight.! !


!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/20/2021 10:24:29'!
drawButton: aPluggableButtonMorph
	aPluggableButtonMorph isRoundButton
		ifTrue: [
			(self drawButtonIcon: aPluggableButtonMorph) ifFalse: [
				self drawButtonRound: aPluggableButtonMorph ]]
		ifFalse: [
			self drawButton3D: aPluggableButtonMorph.
			self drawButtonIcon: aPluggableButtonMorph ]! !


!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 7/20/2021 12:49:42'!
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


!Theme methodsFor: 'other options' stamp: 'jmv 7/20/2021 11:12:18'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^14! !

!methodRemoval: Theme #buttonGradientHeight stamp: 'jmv 7/20/2021 12:44:44'!
Theme removeSelector: #buttonGradientHeight!
!methodRemoval: Theme #buttonGradientBottomFactor stamp: 'jmv 7/20/2021 12:44:54'!
Theme removeSelector: #buttonGradientBottomFactor!
!methodRemoval: Theme #buttonGradientTopFactor stamp: 'jmv 7/20/2021 12:44:59'!
Theme removeSelector: #buttonGradientTopFactor!
!methodRemoval: Theme #useButtonGradient stamp: 'jmv 7/20/2021 12:44:59'!
Theme removeSelector: #useButtonGradient!
!methodRemoval: BitBltCanvas class #steButtonForm: stamp: 'jmv 7/20/2021 12:45:34'!
BitBltCanvas class removeSelector: #steButtonForm:!
!methodRemoval: BitBltCanvas #roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: stamp: 'jmv 7/20/2021 12:45:53'!
BitBltCanvas removeSelector: #roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height:!
!methodRemoval: BitBltCanvas #roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: stamp: 'jmv 7/20/2021 12:45:11'!
BitBltCanvas removeSelector: #roundRect:color:radius:gradientTop:gradientBottom:gradientHeight:!
!methodRemoval: MorphicCanvas #roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: stamp: 'jmv 7/20/2021 12:45:53'!
MorphicCanvas removeSelector: #roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height:!
!methodRemoval: MorphicCanvas #roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: stamp: 'jmv 7/20/2021 12:45:12'!
MorphicCanvas removeSelector: #roundRect:color:radius:gradientTop:gradientBottom:gradientHeight:!
!methodRemoval: MorphicCanvas #windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor: stamp: 'jmv 7/20/2021 12:46:32'!
MorphicCanvas removeSelector: #windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:!
!methodRemoval: MorphicCanvas #drawButtonRoundGradient: stamp: 'jmv 7/20/2021 12:44:38'!
MorphicCanvas removeSelector: #drawButtonRoundGradient:!

!MorphicCanvas reorganize!
('accessing' clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-widgets' drawButton3D: drawButton: drawButtonEmbossedLabel: drawButtonIcon: drawButtonRegularLabel: drawButtonRound:)
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
('morphic' boundingRectOfCurrentMorphAfterDraw canDraw: drawCurrentAndSubmorphs morph:isAtPoint:)
!

