'From Cuis 5.0 [latest update: #4610] on 26 May 2021 at 12:04:38 pm'!

!HaloMorph methodsFor: 'drawing' stamp: 'jmv 5/26/2021 11:17:25'!
drawCoordinateSystemOn: aCanvas

	| x0 x1 y0 y1 c stepX stepY haloTargetTx prevTx |
	haloTargetTx _ MorphicTranslation identity.
	target allOwnersReverseDo: [ :o | haloTargetTx _ haloTargetTx composedWith: o location ].
	haloTargetTx _ haloTargetTx composedWith: target location.

	x0 _ x1 _ y0 _ y1 _ 0.
	target displayFullBounds corners collect: [ :pt | | p |
		p _ haloTargetTx inverseTransform: pt .
		x0 _ x0 min: p x.
		x1 _ x1 max: p x.
		y0 _ y0 min: p y.
		y1 _ y1 max: p y.].
	x1-x0 < 1000 ifTrue: [
		stepX _ 50.
		stepY _ 20 ]
	ifFalse: [
		stepX _ 100.
		stepY _ 50 ].

	prevTx _ aCanvas currentTransformation.
	aCanvas geometryTransformation: haloTargetTx.

	c _ `Color black alpha: 0.6`.
	aCanvas line: x0@0 to: x1@0 width: 2 color: c.
	aCanvas line: 0@y0 to: 0@y1 width: 2 color: c.

	(x0 truncateTo: stepX) to: (x1 - stepX truncateTo: stepX) by: stepX do: [ :x |
		aCanvas line: x @ -5 to: x @ 5 width: 2 color: c.
	   	aCanvas drawString: x printString atCenterXBaselineY: x @ -10 font: nil color: c ].
	  aCanvas drawString: 'x' atCenterX: x1 - 15 @ 0 font: nil color: c.

	(y0 truncateTo: stepY) to: (y1 - stepY truncateTo: stepY) by: stepY do: [ :y |
		aCanvas line: -5 @ y to: 5 @ y width: 2 color: c.
	   	aCanvas drawString: y printString, ' ' atWaistRight: -5 @ y font: nil color: c ].
	  aCanvas drawString: 'y' atWaistRight: -5 @ (y1 - 20) font: nil color: c.

	aCanvas geometryTransformation: prevTx.! !

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 5/26/2021 09:02:31'!
containsGlobalPoint: worldPoint

	self visible ifTrue: [
		self topmostWorld ifNotNil: [ :w |
			^self morphLocalBounds containsPoint:
				(self internalizeFromWorld: worldPoint) ]].
	^ false! !

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 5/26/2021 09:03:21'!
requiresVectorCanvas

	target ifNotNil: [
		^target requiresVectorCanvas ].
	^false! !


!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 5/26/2021 08:44:50'!
geometryTransformation: aGeometryTransformation
	"Only used for testing. Usually, transformations are handled as we traverse the Morphs tree."

	currentTransformation _ aGeometryTransformation.! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/26/2021 11:00:07'!
drawString: s atCenterXBaselineY: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font |
	font _ self fontToUse: fontOrNil.
	^self
		drawString: s
		from: 1 to: s size
		atBaseline: pt - ((font widthOfString: s) / 2 @ 0)
		font: font color: aColor! !

!MorphicCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 11:25:46'!
frameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 10:15:12'!
frameGlobalRect: rect borderWidth: borderWidth color: borderColor
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 09:54:36'!
reverseGlobalRectangleBorder: r borderWidth: borderWidth
	self subclassResponsibility.! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 5/26/2021 11:31:02'!
drawHighlight: aMorph

	aMorph displayBounds ifNotNil: [ :r |
		self
			frameAndFillGlobalRect: r
			fillColor: `Color pink alpha: 0.2`
			borderWidth: 4
			color: `Color black` ].! !


!BitBltCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 11:26:23'!
frameAndFillGlobalRect: rect fillColor: fillColor borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		frameAndFillGlobalRect: (10@10 extent: 300@200)
		fillColor: Color green
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	engine ifNil: [ ^nil ].
	engine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth;
		combinationRule: (fillColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: fillColor;
		fillRect: (rect insetBy: borderWidth).
		! !

!BitBltCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 10:25:18'!
frameGlobalRect: rect borderWidth: borderWidth color: borderColor
	"
	(BitBltCanvas onForm: Display)
		frameGlobalRect: (10@10 extent: 300@200)
		borderWidth: 20
		color: Color red.
	Display forceToScreen
	"
	engine ifNil: [ ^nil ].
	engine
		combinationRule: (borderColor isOpaque ifTrue: [Form paint] ifFalse: [Form blend]);
		fillColor: borderColor;
		frameRect: rect borderWidth: borderWidth.! !

!BitBltCanvas methodsFor: 'drawing - Global Coordinates' stamp: 'jmv 5/26/2021 10:18:16'!
reverseGlobalRectangleBorder: rect borderWidth: borderWidth
	"
	(BitBltCanvas onForm: Display)
		reverseGlobalRectangleBorder: (10@10 extent: 300@200)
		borderWidth: 20.
	Display forceToScreen
	"
	engine ifNil: [ ^nil ].
	engine
		sourceForm: nil;
		fillColor: `Color gray`;
		combinationRule: Form reverse;
		frameRect: rect borderWidth: borderWidth! !


!Morph methodsFor: 'geometry testing' stamp: 'jmv 5/25/2021 20:24:49'!
fullContainsGlobalPoint: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape."

	self visible ifTrue: [
		self topmostWorld ifNotNil: [ :w |
			(self containsGlobalPoint: worldPoint) ifTrue: [ ^ true ].
			self submorphsDo: [ :m |
				(m fullContainsGlobalPoint: worldPoint) ifTrue: [ ^ true ]]]].
	^ false! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 5/26/2021 10:43:15'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].

	Preferences halosShowCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas frameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.2`.
			b _ target displayBounds.
			b area > 0 ifTrue: [
				aCanvas frameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.4` ].
			self drawCoordinateSystemOn: aCanvas ]].! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 5/26/2021 11:09:15'!
addHandles: aDisplayRectangle
	"update my size. owner is world, therefore owner coordinates are Display coordinates."

	self morphBounds: (aDisplayRectangle outsetBy: 30@30).
	haloBox _ self handlesBox.
	target addHandlesTo: self box: haloBox.
	self addNameString: (target printStringLimitedTo: 40).
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 5/26/2021 10:55:19'!
doResize: evt with: resizeHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"MUST fix resize / rescale / rotation of embedded morphs!!!!!!"
newExtent _ evt eventPosition - positionOffset - target morphPositionInWorld.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtentInWorld: newExtent.
	resizeHandle morphPositionInWorld: evt eventPosition - (resizeHandle morphExtent // 2).	
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 5/26/2021 10:55:23'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable."

	| radians |
"MUST fix resize / rescale / rotation of embedded morphs!!!!!!"
	evt hand obtainHalo: self.
	radians _ (evt eventPosition - target referencePosition) theta + angleOffset.
	radians _ radians detentBy: 0.05 atMultiplesOf: Float pi / 4 snap: false.
	radians = 0.0
		ifTrue: [rotHandle color: `Color lightBlue`]
		ifFalse: [rotHandle color: `Color blue`].
	rotHandle submorphsDo:
		[:m | m color: rotHandle color makeForegroundColor].
	target rotation: radians.
	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2).	
	self redrawNeeded.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 5/26/2021 10:55:25'!
doScale: evt with: scaleHandle
	"Update the scale of my target if it is scalable."

	| scale |
"MUST fix resize / rescale / rotation of embedded morphs!!!!!!"
	evt hand obtainHalo: self.
	scale _ (evt eventPosition - target referencePosition) rho * scaleOffset.
	scale = 1.0
		ifTrue: [scaleHandle color: `Color lightBlue`]
		ifFalse: [scaleHandle color: `Color blue`].
	scaleHandle submorphsDo:
		[:m | m color: scaleHandle color makeForegroundColor].
	target scale: scale.
	scaleHandle morphPositionInWorld: evt eventPosition - (scaleHandle morphExtent // 2).
	self redrawNeeded.! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 5/26/2021 09:30:19'!
drawCurrentAndSubmorphs
	
	self isCurrentMorphVisible ifTrue: [

		currentMorph drawOn: self.
		currentMorph displayBoundsSetFrom: self.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		currentMorph postDrawOn: self.
		currentMorph displayBoundsUpdateFrom: self.

		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ].
	].! !

!methodRemoval: BitBltCanvas #drawCoordinateSystem: stamp: 'jmv 5/26/2021 11:55:10'!
BitBltCanvas removeSelector: #drawCoordinateSystem:!
!methodRemoval: BitBltCanvas #drawCurrentMorphHighlight stamp: 'jmv 5/26/2021 12:02:24'!
BitBltCanvas removeSelector: #drawCurrentMorphHighlight!

!MorphicCanvas reorganize!
('accessing' clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-icons' drawButtonIconFromCurrentMorph)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: reverseGlobalRectangleBorder:borderWidth:)
('initialization' initializeWith:origin: world:)
('morphic' canDraw: clipCurrentMorph: currentMorphDrawingFails drawCurrentAndSubmorphs drawCurrentAsError drawCurrentAsOutline drawHighlight: fullDraw: fullDrawHand: into: outOfMorph restorePatch:bounds: savePatch:bounds:)
('other' showAt:invalidRects:)
('printing' printOn:)
('testing' drawsOnDisplay isCurrentMorphVisible)
('private' boundingRectOfCurrentMorphAfterDraw boundingRectOfCurrentMorphFromLocalBounds canvasOrigin canvasToUse clippingByCurrentMorphDo: externalizeDisplayBounds:from: fontToUse: setClipRect: setForm: updatingMorphBoundsDo:)
('morphic world' computeDamage:repair:rootMorphs:rootMorphsDamage: drawWorld:repair: drawWorld:rootMorphs:rootMorphsDamage:allDamage: drawWorldBackground:rects: fullAddCurrentRect:submorphs:to: fullAddRedrawRect:to: fullUpdateBounds: fullUpdateCurrentBounds updateHandsDisplayBounds:)
!


!BitBltCanvas reorganize!
('drawing' line:to:width:color:)
('drawing-icons' drawButtonIconFromCurrentMorph)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:topLeftColor:bottomRightColor:borderWidth: reverseRectangleBorder:borderWidth:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:from:to:atBaseline:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: reverseGlobalRectangleBorder:borderWidth:)
('initialization')
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: fontToUse: frameRectangle:borderWidth:topLeftColor:bottomRightColor: setClipRect: setForm: setPaintColor: updatingMorphBoundsDo:)
('morphic' boundingRectOfCurrentMorphAfterDraw canDraw: drawCurrentAndSubmorphs morph:isAtPoint:)
('testing')
!

