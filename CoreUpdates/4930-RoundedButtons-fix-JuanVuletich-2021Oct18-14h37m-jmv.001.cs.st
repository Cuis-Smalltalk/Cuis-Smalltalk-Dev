'From Cuis 5.0 [latest update: #4933] on 18 October 2021 at 2:46:00 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 10/18/2021 11:58:24'!
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
		r _ r min: (rect width min: rect height) * 0.5.
		aCanvas roundRect: rect color: colorForButton radius: r ].

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !


!BitBltCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 10/18/2021 14:45:01'!
roundRect: aRectangle color: aColor radius: aNumber
	"
	(BitBltCanvas onForm: Display) roundRect: (10@10 extent: 200@100) color: Color red radius: 10.5. Display forceToScreen.
	"
	| r |
	r _ (currentTransformation externalizeScalar: aNumber) rounded.

	"top stripe"
	self
		image: (self class topLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self fillRectangle: ((aRectangle withHeight: r) insetBy: r@0) color: aColor.

	"center stripe"
	self fillRectangle: (aRectangle insetBy: (0 @ r corner: 0 @ r)) color: aColor.
	
	"bottom stripe"
	self
		image: (self class bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (self class bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: aColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@r negated)) extent: (aRectangle width - r - r@r)) color: aColor! !

