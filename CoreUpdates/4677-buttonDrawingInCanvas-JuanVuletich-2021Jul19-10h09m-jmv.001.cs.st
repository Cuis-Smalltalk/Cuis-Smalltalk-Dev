'From Cuis 5.0 [latest update: #4677] on 19 July 2021 at 10:14:17 am'!

!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 7/18/2021 19:23:08'!
label
	^label! !


!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/18/2021 19:39:56'!
drawButton3D: aPluggableButtonMorph

	| borderStyleSymbol c |
	borderStyleSymbol _ aPluggableButtonMorph isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ aPluggableButtonMorph color.
	aPluggableButtonMorph mouseIsOver ifTrue: [ c _ c  lighter ].
	self
		fillRectangle: aPluggableButtonMorph morphLocalBounds
		color: c
		borderWidth: aPluggableButtonMorph borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawButtonRegularLabel: aPluggableButtonMorph.! !

!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/18/2021 19:33:54'!
drawButton: aPluggableButtonMorph
	aPluggableButtonMorph isRoundButton
		ifTrue: [
			(self drawButtonIcon: aPluggableButtonMorph) ifFalse: [
				self drawButtonRoundGradient: aPluggableButtonMorph ]]
		ifFalse: [
			self drawButton3D: aPluggableButtonMorph.
			self drawButtonIcon: aPluggableButtonMorph ]! !

!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/18/2021 19:34:08'!
drawButtonEmbossedLabel: aPluggableButtonMorph

	| availableW center colorForLabel f l labelMargin targetSize w x y label |
	label _ aPluggableButtonMorph label.
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		aPluggableButtonMorph isPressed
			ifFalse: [
				aPluggableButtonMorph mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ aPluggableButtonMorph fontToUse.
		center _ aPluggableButtonMorph morphExtent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ aPluggableButtonMorph morphExtent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f lineSpacing // 2).
		self
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]! !

!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/18/2021 19:34:59'!
drawButtonIcon: aPluggableButtonMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	self subclassResponsibility! !

!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/18/2021 19:34:29'!
drawButtonRegularLabel: aPluggableButtonMorph

	| w f center x y  availableW l labelMargin label |

	label _ aPluggableButtonMorph label.
	f _ aPluggableButtonMorph fontToUse.
	center _ aPluggableButtonMorph morphExtent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ aPluggableButtonMorph morphExtent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f lineSpacing // 2).
		aPluggableButtonMorph isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		self
			drawString: l
			at: x@y
			font: f
			color: Theme current buttonLabel ]! !

!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/18/2021 19:34:36'!
drawButtonRoundGradient: aPluggableButtonMorph
	| r colorForButton rect bottomFactor topFactor color |
	color _ aPluggableButtonMorph color.
	aPluggableButtonMorph isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			aPluggableButtonMorph mouseIsOver
				ifTrue: [	
					colorForButton _ Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [
					colorForButton _ color ]]
		ifTrue: [
			topFactor _ Theme current buttonGradientBottomFactor.
			bottomFactor _ Theme current buttonGradientTopFactor.
			colorForButton _ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		Theme current useButtonGradient
			ifTrue: [
				rect _ aPluggableButtonMorph morphLocalBounds insetBy: `1@3`.
				self
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ aPluggableButtonMorph morphLocalBounds insetBy: `1@3`.
				self roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawButtonEmbossedLabel: aPluggableButtonMorph ]
		ifFalse: [ self drawButtonRegularLabel: aPluggableButtonMorph ].! !


!BitBltCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/18/2021 19:29:05'!
drawButtonIcon: aPluggableButtonMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	aPluggableButtonMorph magnifiedIcon ifNotNil: [ :theIcon | | iconColor |
		iconColor _ aPluggableButtonMorph isPressed
			ifTrue: [ `Color gray: 0.75` ]
			ifFalse: [
				aPluggableButtonMorph mouseIsOver
					ifTrue: [ `Color gray: 0.75` ]
					ifFalse: [ `Color white` ]].
		self
			image: theIcon
			multipliedBy: iconColor
			at: (aPluggableButtonMorph morphExtent - theIcon extent //2).
		^true ].
	^false! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 7/18/2021 19:09:00'!
drawOn: aCanvas

	aCanvas drawButton: self! !

!methodRemoval: BitBltCanvas #drawButtonIconFromCurrentMorph stamp: 'jmv 7/19/2021 10:10:23'!
BitBltCanvas removeSelector: #drawButtonIconFromCurrentMorph!
!methodRemoval: MorphicCanvas #drawButtonIconFromCurrentMorph stamp: 'jmv 7/19/2021 10:10:23'!
MorphicCanvas removeSelector: #drawButtonIconFromCurrentMorph!
!methodRemoval: PluggableButtonMorph #draw3DLookOn: stamp: 'jmv 7/19/2021 10:10:58'!
PluggableButtonMorph removeSelector: #draw3DLookOn:!
!methodRemoval: PluggableButtonMorph #iconColor stamp: 'jmv 7/19/2021 10:11:06'!
PluggableButtonMorph removeSelector: #iconColor!
!methodRemoval: PluggableButtonMorph #drawRoundGradientLookOn: stamp: 'jmv 7/19/2021 10:11:13'!
PluggableButtonMorph removeSelector: #drawRoundGradientLookOn:!
!methodRemoval: PluggableButtonMorph #drawEmbossedLabelOn: stamp: 'jmv 7/19/2021 10:11:19'!
PluggableButtonMorph removeSelector: #drawEmbossedLabelOn:!
!methodRemoval: PluggableButtonMorph #drawRegularLabelOn: stamp: 'jmv 7/19/2021 10:11:34'!
PluggableButtonMorph removeSelector: #drawRegularLabelOn:!

!MorphicCanvas reorganize!
('accessing' clipRect currentTransformation extent form geometryTransformation: newClipRect:)
('drawing' line:to:width:color:)
('drawing-images' image:at: image:at:sourceRect: image:multipliedBy:at: stencil:at:color: stencil:at:sourceRect:color:)
('drawing-ovals' ellipseCenter:radius:borderWidth:borderColor:fillColor: ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:)
('drawing-rectangles' fillRectangle:color: fillRectangle:color:borderWidth:borderStyleSymbol:baseColorForBorder: fillRectangle:tilingWith:multipliedBy: frameAndFillRectangle:fillColor:borderWidth:borderColor: frameRectangle:borderWidth:color: frameRectangle:color:borderWidth:borderStyleSymbol:)
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:at:font:color: drawString:at:font:color:embossed: drawString:atBaseline:font:color: drawString:atCenterX:font:color: drawString:atCenterXBaselineY:font:color: drawString:atWaist:font:color: drawString:atWaistRight:font:color: drawString:from:to:at:font:color: drawString:from:to:atBaseline:font:color: drawString:from:to:atWaist:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-widgets' drawButton3D: drawButton: drawButtonEmbossedLabel: drawButtonIcon: drawButtonRegularLabel: drawButtonRoundGradient:)
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
('drawing-roundedRectangles' roundRect:color:radius: roundRect:color:radius:gradientTop:gradientBottom:gradientHeight: roundRect:color:radius:gradientTop:gradientCenter:gradientBottom:gradient1Height: windowFrame:color:radius:border:labelHeight:gradientTop:gradientBottom:insideColor:)
('drawing-text' drawString:from:to:atBaseline:font:color: drawStringEmbossed:from:to:at:font:color: textComposition:bounds:color:selectionColor:)
('drawing-widgets' drawButtonIcon:)
('drawing - Global Coordinates' frameAndFillGlobalRect:fillColor:borderWidth:color: frameGlobalRect:borderWidth:color: frameReverseGlobalRect:borderWidth:)
('private' buildAuxWith:multipliedWith: fillRectangle:tilingWith:sourceRect:rule: fontToUse: frameRectangle:borderWidth:topLeftColor:bottomRightColor: setClipRect: setForm:preferSubPixelAntiAliasing: setPaintColor: updatingMorphBoundsDo:)
('morphic' boundingRectOfCurrentMorphAfterDraw canDraw: drawCurrentAndSubmorphs morph:isAtPoint:)
!

