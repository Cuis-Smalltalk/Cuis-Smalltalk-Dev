'From Cuis 5.0 [latest update: #4687] on 20 July 2021 at 11:02:51 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'pb 3/17/2020 13:42:18'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: self morphLocalBounds
		color: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol
		baseColorForBorder: c.

	self drawRegularLabelOn: aCanvas! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/24/2019 20:58:58'!
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
		center _ extent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin.
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
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/24/2019 20:59:05'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ extent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f lineSpacing // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: Theme current buttonLabel ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2021 22:47:56'!
drawRoundLookOn: aCanvas
	| r colorForButton rect |
	colorForButton _ self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 Color h: color hue s: color saturation * 1.3 v: color brightness * 0.9 ]
				ifFalse: [ color ]]
		ifTrue: [ color adjustSaturation: 0.1 brightness: -0.1 ].

	colorForButton ifNotNil: [
		r _ Theme current roundedButtonRadius.
		rect _ self morphLocalBounds insetBy: `1@3`.
		aCanvas roundRect: rect color: colorForButton radius: r ].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !


!MorphicCanvas methodsFor: 'drawing-widgets' stamp: 'jmv 7/20/2021 22:51:58'!
drawButtonIconFromCurrentMorph
	"We assume that we are drawing a PluggableButtonMorph,
	or some morph that conforms the required protocol.
	Answer true if we were able to draw it."

	currentMorph iconDrawSelector ifNotNil: [ :selector |
		self perform: selector.
		^true ].

	currentMorph magnifiedIcon ifNotNil: [ :theIcon |
		self pvtDrawButtonFaceForm: theIcon.
		^true ].

	^false! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 7/20/2021 22:52:40'!
drawOn: aCanvas

	self isRoundButton
		ifTrue: [
			aCanvas drawButtonIconFromCurrentMorph ifFalse: [
				self drawRoundLookOn: aCanvas ]]
		ifFalse: [
			self draw3DLookOn: aCanvas.
			aCanvas drawButtonIconFromCurrentMorph ].! !

!methodRemoval: MorphicCanvas #drawButtonRegularLabel: stamp: 'jmv 7/20/2021 22:47:10'!
MorphicCanvas removeSelector: #drawButtonRegularLabel:!
!methodRemoval: MorphicCanvas #drawButtonIcon: stamp: 'jmv 7/20/2021 22:52:50'!
MorphicCanvas removeSelector: #drawButtonIcon:!
!methodRemoval: MorphicCanvas #drawButton3D: stamp: 'jmv 7/20/2021 22:36:48'!
MorphicCanvas removeSelector: #drawButton3D:!
!methodRemoval: MorphicCanvas #drawButton: stamp: 'jmv 7/20/2021 22:38:11'!
MorphicCanvas removeSelector: #drawButton:!
!methodRemoval: MorphicCanvas #drawButtonEmbossedLabel: stamp: 'jmv 7/20/2021 22:48:00'!
MorphicCanvas removeSelector: #drawButtonEmbossedLabel:!
!methodRemoval: MorphicCanvas #drawButtonRound: stamp: 'jmv 7/20/2021 22:36:44'!
MorphicCanvas removeSelector: #drawButtonRound:!
!methodRemoval: PluggableButtonMorph #drawButtonRoundOn: stamp: 'jmv 7/20/2021 22:35:23'!
PluggableButtonMorph removeSelector: #drawButtonRoundOn:!
