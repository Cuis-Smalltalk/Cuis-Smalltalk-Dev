'From Cuis 6.0 [latest update: #5103] on 8 April 2022 at 10:33:32 am'!

!MorphicCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:19:20'!
privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:20:16'!
privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:21:02'!
privateFrameReverseGlobalRect: r borderWidth: borderWidth
	self subclassResponsibility.! !


!BitBltCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:19:32'!
privateFrameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		privateFrameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color green
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	engine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
		! !

!BitBltCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:20:24'!
privateFrameGlobalRect: rect borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		privateFrameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"

	engine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.! !

!BitBltCanvas methodsFor: 'private-framework' stamp: 'jmv 4/8/2022 10:21:09'!
privateFrameReverseGlobalRect: rect borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)
		privateFrameReverseGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	engine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 4/8/2022 10:20:48'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].
	"Debugging aid"
	"aCanvas
		fillRectangle: haloBox
		color: (Color red alpha: 0.2).
	aCanvas
		fillRectangle: self morphLocalBounds
		color: (Color gray alpha: 0.4)."
	target haloShowsCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas privateFrameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.1`.
			b _ target displayBounds.
			b = fb ifFalse: [
				aCanvas privateFrameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.1` ].
			self drawCoordinateSystemOn: aCanvas ]].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2022 10:21:25'!
drawCurrentAsOutline

	currentMorph visible ifTrue: [
		self displayBoundsSetOnCurrentMorph.
		self contourClearOnCurrentMorph.
		self privateFrameReverseGlobalRect: currentMorph displayBounds borderWidth: 2 ].! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2022 10:19:50'!
drawHighlight: aMorph

	aMorph displayBounds ifNotNil: [ :r |
		self
			privateFrameAndFillGlobalRect: r
			fillColor: `Color pink alpha: 0.2`
			borderWidth: 4
			color: `Color black` ].! !

!methodRemoval: BitBltCanvas #frameGlobalRect:borderWidth:color: stamp: 'jmv 4/8/2022 10:31:12'!
BitBltCanvas removeSelector: #frameGlobalRect:borderWidth:color:!
!methodRemoval: BitBltCanvas #frameAndFillGlobalRect:fillColor:borderWidth:color: stamp: 'jmv 4/8/2022 10:31:12'!
BitBltCanvas removeSelector: #frameAndFillGlobalRect:fillColor:borderWidth:color:!
!methodRemoval: BitBltCanvas #frameReverseGlobalRect:borderWidth: stamp: 'jmv 4/8/2022 10:31:12'!
BitBltCanvas removeSelector: #frameReverseGlobalRect:borderWidth:!
!methodRemoval: MorphicCanvas #frameReverseGlobalRect:borderWidth: stamp: 'jmv 4/8/2022 10:31:12'!
MorphicCanvas removeSelector: #frameReverseGlobalRect:borderWidth:!
!methodRemoval: MorphicCanvas #frameGlobalRect:borderWidth:color: stamp: 'jmv 4/8/2022 10:31:12'!
MorphicCanvas removeSelector: #frameGlobalRect:borderWidth:color:!
!methodRemoval: MorphicCanvas #frameAndFillGlobalRect:fillColor:borderWidth:color: stamp: 'jmv 4/8/2022 10:31:12'!
MorphicCanvas removeSelector: #frameAndFillGlobalRect:fillColor:borderWidth:color:!

!MorphicCanvas reorganize!
('accessing' boundsFinderCanvas clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-images' image:at:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistCenter:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: drawTextComposition:at:extent:color:selectionColor: drawTextComposition:at:extent:color:selectionColor:avoidOverhang: textComposition:bounds:color:selectionColor: textComposition:bounds:color:selectionColor:avoidOverhang:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawExpandedAt: drawLeftIcon drawMenuIcon drawNotExpandedAt: drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-widgets' drawButtonIconFromCurrentMorph)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight:)
('initialization' initializeWithTranslation: resetCanvas world:)
('morphic' clipCurrentMorph: contourClearOnCurrentMorph currentMorphDrawingFails displayBoundsSetOnCurrentMorph displayBoundsUpdateOnCurrentMorph drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline drawHand drawHighlight: fullDraw: fullDrawHand: into: outOfMorph)
('other' showAt:invalidRect:)
('printing' printOn:)
('testing' canDoVectorGraphics drawsOnDisplay isBoundsFinderCanvas usesVectorEnginePlugin)
('private' canvasOrigin canvasToUse clippingByCurrentMorphDo: fontToUse: setClipRect:)
('private-framework' privateFrameAndFillGlobalRect:fillColor:borderWidth:color: privateFrameGlobalRect:borderWidth:color: privateFrameReverseGlobalRect:borderWidth:)
('morphic world' boundingRectOfCurrentMorphAfterDraw computeDamage:repair:rootMorphs:rootMorphsDamage: drawWorld:repair: drawWorld:rootMorphs:rootMorphsDamage:backgroundDamage: drawWorldBackground:rects:)
!


!BitBltCanvas reorganize!
('accessing' boundsFinderCanvas)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:topLeftColor:bottomRightColor:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius:)
('drawing-text' drawString:from:to:atBaseline:font:color: drawStringEmbossed:from:to:at:font:color:)
('drawing-icons' drawCloseIcon drawCollapseIcon drawDownIcon drawExpandIcon drawExpandedAt: drawLeftIcon drawMenuIcon drawNotExpandedAt: drawPushPinIcon drawRightIcon drawUpIcon)
('drawing-windows' drawRoundedFrame:border:color:insideColor:labelHeight: roundBottomLeftCornerX:y:length:border:color: roundBottomRightCornerX:y:length:border:color: roundEdge:border:color: roundTopLeftCornerX:y:length:border:color: roundTopRightCornerX:y:length:border:color: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('initialization' initializeWithTranslation: resetCanvas world:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: fontToUse: frameRectangle:borderWidth:topLeftColor:bottomRightColor: pvtDrawButtonFaceForm: setClipRect: setForm:subPixelAntiAliasing: setPaintColor:)
('private-framework' privateFrameAndFillGlobalRect:fillColor:borderWidth:color: privateFrameGlobalRect:borderWidth:color: privateFrameReverseGlobalRect:borderWidth:)
('morphic' drawCurrentAndSubmorphs drawHand restorePatch:bounds: savePatch:bounds:)
!

