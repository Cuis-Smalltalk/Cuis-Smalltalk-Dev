'From Cuis 5.0 [latest update: #4428] on 26 October 2020 at 11:30:04 am'!

!LabelMorph methodsFor: 'drawing' stamp: 'KenD 10/23/2020 13:37:36'!
drawOn: aCanvas
	aCanvas
		drawString: (contents ifNil: [ '' ])
		at: self morphTopLeft  + borderWidth
		font: self fontToUse
		color: color.
	(borderWidth > 0) ifTrue: [
		aCanvas
			frameRectangle: (0@0 extent: extent)
			color: borderColor
			borderWidth: borderWidth
			borderStyleSymbol: #simple ]! !

