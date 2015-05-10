'From Cuis 4.2 of 25 July 2013 [latest update: #2280] on 1 May 2015 at 4:22:21.740889 pm'!

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/1/2015 16:19'!
drawString: s at: pt font: fontOrNil color: aColor embossed: aBoolean

	aBoolean
		ifTrue: [ self drawStringEmbossed: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]
		ifFalse: [ self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor ]! !


!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/1/2015 16:20'!
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
		y _ center y - (f height // 2).
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 5/1/2015 16:21'!
drawLabelOn: aCanvas

	| e x0 y0 f w availableW l |
	e _ self boxExtent.
	x0 _  e x * 4 + 14.
	y0 _ 2.
	f _ Preferences windowTitleFont.
	w _ f widthOfString: labelString.
	availableW _ extent x - x0 - 10.
	availableW >= w
		ifTrue: [
			l _ labelString ]
		ifFalse: [
			l _ labelString squeezedTo: (1.0 * labelString size * availableW / w) rounded ].
	aCanvas
		drawString: l
		at: x0@y0
		font: f
		color: Theme current windowLabel
		embossed: Theme current embossedTitles! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/1/2015 16:18'!
drawString: s at: pt font: fontOrNil color: aColor

	self drawString: s from: 1 to: s size at: pt font: fontOrNil color: aColor! !

!methodRemoval: MorphicCanvas #drawStringEmbossed:at:font:color:!
MorphicCanvas removeSelector: #drawStringEmbossed:at:font:color:!
