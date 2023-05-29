'From Cuis 6.0 [latest update: #5815] on 26 May 2023 at 7:23:12 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'len 5/26/2023 19:21:40'!
enableLabelColorWith: aColor
	^ self isEnabled 
		ifTrue: [aColor] ifFalse: [aColor paler "twiceLighter" ]! !


!LayoutMorph methodsFor: 'accessing' stamp: 'len 5/26/2023 18:57:28'!
adoptWidgetsColor: paneColor
	super adoptWidgetsColor: paneColor.
	doAdoptWidgetsColor
		ifTrue: [ self color: paneColor ]
		ifFalse: [ self color: `Color transparent` ]! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'len 5/26/2023 19:20:38'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel := self enableLabelColorWith: Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel := colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel := colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f := self fontToUse.
		center := extent // 2.
		labelMargin := 3.
		w := f widthOfString: label.
		availableW := extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l := label ]
			ifFalse: [
				x := labelMargin.
				targetSize := label size * availableW // w.
				l := label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize := targetSize - 1.
					l := label squeezedTo: targetSize ]].
		
		w := f widthOfString: l.
		x := center x - (w // 2).
		y := center y - (f lineSpacing // 2).
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'len 5/26/2023 19:20:30'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f := self fontToUse.
	center := extent // 2.

	label ifNotNil: [
		labelMargin := 4.
		w := f widthOfString: label.
		availableW := extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x := center x - (w // 2).
				l := label ]
			ifFalse: [
				x := labelMargin.
				l := label squeezedTo: (label size * availableW / w) rounded ].
		y := center y - (f lineSpacing // 2).
		self isPressed ifTrue: [
			x := x + 1.
			y := y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: (self enableLabelColorWith: Theme current buttonLabel) ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'len 5/26/2023 19:15:17'!
drawRoundLookOn: aCanvas
	| r colorForButton rect |
	colorForButton := self isPressed
		ifFalse: [
			self mouseIsOver
				ifTrue: [	 mouseOverColor ifNil: [ Color h: color hue s: color saturation * 1.2 v: color brightness * 0.97 ]]
				ifFalse: [ color ]]
		ifTrue: [ mouseOverColor ifNil: [ color adjustSaturation: -0.15 brightness: 0.05 ]].

	r := Theme current roundedButtonRadius.
	rect := self morphLocalBounds insetBy: `3@3`.
	r := r min: (rect width min: rect height) * 0.5.
	aCanvas roundRect: rect color: (self enableColorWith: colorForButton) radius: r.

	Theme current embossedButtonLabels
		ifTrue: [ self drawEmbossedLabelOn: aCanvas ]
		ifFalse: [ self drawRegularLabelOn: aCanvas ].! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'len 5/26/2023 19:21:30'!
enableColorWith: aColor
	^ self isEnabled 
		ifTrue: [aColor] ifFalse: [aColor duller]! !

