'From Cuis 5.0 [latest update: #4920] on 13 October 2021 at 2:55:43 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 10/13/2021 14:55:36'!
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
		rect _ self morphLocalBounds insetBy: `3@3`.
		aCanvas roundRect: rect color: colorForButton radius: r ].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !

!methodRemoval: DraggeableButtonMorph #isRoundButton stamp: 'jmv 10/13/2021 14:47:22'!
DraggeableButtonMorph removeSelector: #isRoundButton!
