'From Cuis 6.0 [latest update: #5828] on 29 May 2023 at 4:36:34 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'len 5/26/2023 19:44:53'!
drawRoundLookOn: aCanvas
	| r colorForButton rect |
	colorForButton := self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 mouseOverColor ifNil: [ Color h: color hue s: color saturation * 1.2 v: color brightness * 0.97 ]]
				ifFalse: [ color ]]
		ifTrue: [ mouseOverColor ifNil: [ color adjustSaturation: 0.1 brightness: -0.15 ]].

	r := Theme current roundedButtonRadius.
	rect := self morphLocalBounds insetBy: `3@3`.
	r := r min: (rect width min: rect height) * 0.5.
	aCanvas roundRect: rect color: (self enableColorWith: colorForButton) radius: r.

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !

