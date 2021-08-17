'From Cuis 5.0 [latest update: #4758] on 16 August 2021 at 3:21:11 pm'!

!WidgetMorph methodsFor: 'drawing' stamp: 'jmv 8/16/2021 15:20:50'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self morphLocalBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.! !

