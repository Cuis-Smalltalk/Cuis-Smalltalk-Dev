'From Cuis 6.0 [latest update: #5822] on 29 May 2023 at 1:51:37 pm'!

!LayoutMorph methodsFor: 'accessing' stamp: 'jmv 3/10/2018 22:12:27'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor
		ifTrue: [ self color: (Theme current buttonColorFrom: paneColor) ]
		ifFalse: [ self color: `Color transparent` ]! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/29/2023 13:49:44'!
drawRoundLookOn: aCanvas
	| r colorForButton rect |
	colorForButton := self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 mouseOverColor ifNil: [ Color h: color hue s: color saturation * 1.2 v: color brightness * 0.92 ]]
				ifFalse: [ color ]]
		ifTrue: [ mouseOverColor ifNil: [ color adjustSaturation: 0.1 brightness: -0.15 ]].

	r := Theme current roundedButtonRadius.
	rect := self morphLocalBounds insetBy: `3@3`.
	r := r min: (rect width min: rect height) * 0.5.
	aCanvas roundRect: rect color: (self enableColorWith: colorForButton) radius: r.

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !

