'From Cuis 5.0 [latest update: #4984] on 28 November 2021 at 9:34:07 pm'!

!TextParagraphMorph methodsFor: 'drawing' stamp: 'hlsf 11/28/2021 21:30:56'!
drawOn: aCanvas
	aCanvas 
		fillRectangle: self morphLocalBounds color: color.
	aCanvas
		textComposition: textComposition
		bounds: (self morphLocalBounds insetBy: borderWidth * 2)
		color: Theme current text
		selectionColor: `Color red`.
	aCanvas
		frameRectangle: self morphLocalBounds
		color: borderColor
		borderWidth: borderWidth
		borderStyleSymbol: nil! !

