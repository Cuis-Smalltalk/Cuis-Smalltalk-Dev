'From Cuis 5.0 of 7 November 2016 [latest update: #3121] on 15 July 2017 at 11:19:17 pm'!

!PluggableButtonMorph methodsFor: 'as yet unclassified' stamp: 'pb 7/15/2017 23:15:35'!
morphExtent
	"Use extent if it has already been manually set, otherwise try to set it by computing from the label text and font, otherwise try using the icon extent, or finally fall back to the default value."
	^ extent ifNil: [
		extent := (self fontToUse notNil and: [ label notNil ])
			ifTrue: [ "Add a bit of padding"
				(self fontToUse widthOfString: label) + 10 @ (self fontToUse height + 10) ]
			ifFalse: [
				icon
					ifNil: [ `20@15` ]
					ifNotNil: [ icon extent ]]].! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'pb 7/15/2017 22:34:12'!
morphHeight

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ self morphExtent y! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'pb 7/15/2017 22:34:52'!
morphPosition: newPos extent: newExtent
	"Change the position of this morph. Argument is in owner's coordinates."

	| oldBoundsInWorld someChange |

	"Ask for the old bounds before updating them, but ask for repair only if extent or position has really changed."
	oldBoundsInWorld _ self morphBoundsInWorld.
	someChange _ false.
	(location isTranslation: newPos) ifFalse: [
		location _ location withTranslation: newPos.
		someChange _ true ].

	self morphExtent = newExtent ifFalse: [
		(self privateExtent: newExtent) ifTrue: [
			someChange _ true ]].

	someChange ifTrue: [
		"Ask for the old bounds before updating them, but ask for repair only if extent or position has really changed."
		oldBoundsInWorld ifNotNil: [
			self invalidateDisplayRect: oldBoundsInWorld from: nil ].
		self someSubmorphPositionOrExtentChanged.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ]! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'pb 7/15/2017 22:34:20'!
morphWidth

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ self morphExtent x! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'pb 7/15/2017 22:40:30'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: (`0@0` extent: self morphExtent)
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'pb 7/15/2017 22:40:17'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ self morphExtent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ self morphExtent x - labelMargin - labelMargin.
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
		y _ center y - (f height // 2).
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'pb 7/15/2017 22:39:57'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ self morphExtent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ self morphExtent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f height // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: Theme current buttonLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'pb 7/15/2017 22:39:34'!
drawRoundGradientLookOn: aCanvas
	| r colorForButton rect bottomFactor topFactor |

	self isPressed
		ifFalse: [
			topFactor _ Theme current buttonGradientTopFactor.
			bottomFactor _ Theme current buttonGradientBottomFactor.
			self mouseIsOver
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
				rect _ (`0@0` extent: self morphExtent) insetBy: `1@3`.
				aCanvas
					roundRect: rect
					color: colorForButton
					radius: r
					gradientTop: topFactor
					gradientBottom: bottomFactor
					gradientHeight: Theme current buttonGradientHeight ]
			ifFalse: [
				rect _ (`0@0` extent: self morphExtent) insetBy: `1@3`.
				aCanvas roundRect: rect color: colorForButton radius: r ]
		].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ]! !

!PluggableButtonMorph methodsFor: 'initialization' stamp: 'pb 7/15/2017 22:36:46'!
initialize
	"initialize the state of the receiver"
	super initialize.

	roundButtonStyle := nil.	"nil: honor Theme. true: draw as round button. false: draw as classic 3d border square button"
	model := nil.
	getStateSelector := nil.
	actionSelector := nil.
	isPressed := false.
	mouseIsOver := false.
	actWhen := #buttonUp.
	"We are overriding any value populated in extent by our superclass with nil so we know to perform the inital morph extent calculation"
	extent := nil! !

!PluggableButtonMorph methodsFor: 'private' stamp: 'pb 7/15/2017 22:36:28'!
magnifiedIcon
	| factor magnifiedExtent w h |

	icon ifNil: [ ^nil ].
	magnifiedIcon ifNil: [
		magnifiedIcon := icon.
		w := icon width.
		h := icon height.
		w*h = 0 ifFalse: [
			factor _ 1.0 * self morphExtent x / w min: 1.0 * self morphExtent y / h.
			(factor < 1 or: [ factor > 1.7 and: [self isRoundButton]]) ifTrue: [
				magnifiedExtent := (icon extent * factor) rounded.
				magnifiedIcon := icon magnifyTo: magnifiedExtent ]]].
	^magnifiedIcon! !

!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'pb 7/15/2017 22:38:44'!
morphContainsPoint: aLocalPoint

	| iconOrigin |
	((`0@0` extent: self morphExtent) containsPoint: aLocalPoint) ifFalse: [ ^false ].
	^ self isOrthoRectangularMorph or: [
		magnifiedIcon isNil or: [
			iconOrigin := self morphExtent - magnifiedIcon extent // 2.
			(magnifiedIcon isTransparentAt: (aLocalPoint - iconOrigin) rounded) not ]]! !

