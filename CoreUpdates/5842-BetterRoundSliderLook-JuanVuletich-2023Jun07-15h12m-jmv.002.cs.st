'From Cuis 6.0 [latest update: #5841] on 7 June 2023 at 3:32:04 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 6/7/2023 15:10:43'!
roundButtonRect
	^self morphLocalBounds insetBy: 3! !


!DraggeableButtonMorph methodsFor: 'drawing' stamp: 'jmv 6/7/2023 15:23:13'!
roundButtonRect
	"Use full bounds for drawing.
	See #computeSlider and #expandSlider"
	^self morphLocalBounds! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 6/7/2023 15:30:50'!
sliderSlack
	^self buttonExtent // 6! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 6/7/2023 15:13:03'!
drawRoundLookOn: aCanvas
	| r colorForButton rect |
	colorForButton := self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 mouseOverColor ifNil: [ Color h: color hue s: color saturation * 1.2 v: color brightness * 0.92 ]]
				ifFalse: [ color ]]
		ifTrue: [ mouseOverColor ifNil: [ color adjustSaturation: 0.1 brightness: -0.15 ]].

	r := Theme current roundedButtonRadius.
	rect := self roundButtonRect.
	r := r min: (rect width min: rect height) * 0.5.
	aCanvas roundRect: rect color: (self enableColorWith: colorForButton) radius: r.

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 6/7/2023 15:27:37'!
computeSlider

	| buttonRoom delta |
	buttonRoom := upButton visible ifTrue: [self buttonExtent] ifFalse: [0].
	delta := buttonRoom + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth +  delta @ (borderWidth+self sliderSlack) ]
		ifFalse: [
			slider morphPosition: (borderWidth+self sliderSlack) @ (borderWidth + delta) ] ! !

!ScrollBar methodsFor: 'geometry' stamp: 'jmv 6/7/2023 15:27:54'!
expandSlider
	"Compute the new size of the slider (use the old sliderThickness as a minimum)."

	| e |
	e := (self totalSliderRoom * interval) asInteger max: 7.
	slider morphExtent: (self isHorizontal
		ifTrue: [ e @ (self buttonExtent-(self sliderSlack*2)) ]
		ifFalse: [ self buttonExtent-(self sliderSlack*2) @ e ])! !

!ScrollBar methodsFor: 'scrolling' stamp: 'jmv 6/7/2023 15:21:54'!
sliderGrabbedAt: handPositionRelativeToSlider

	| e p |
	grabPosition := handPositionRelativeToSlider.
	self isHorizontal
		ifTrue: [
			e := slider morphWidth @ self buttonExtent.
			p := slider morphPosition x @ borderWidth ]
		ifFalse: [
			e := self buttonExtent @ slider morphHeight.
			p := borderWidth @ slider morphPosition y ].
	sliderShadow
		morphPosition: p extent: e;
		show! !

