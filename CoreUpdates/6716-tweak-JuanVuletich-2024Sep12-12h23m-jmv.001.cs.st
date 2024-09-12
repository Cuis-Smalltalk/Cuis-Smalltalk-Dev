'From Cuis7.1 [latest update: #6715] on 12 September 2024 at 12:23:55 pm'!

!BorderedBoxMorph methodsFor: 'drawing' stamp: 'jmv 9/12/2024 12:23:42'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.

	"Make the existence of the normal invisible padding evident.
	This is usually not done, and subclasses may assume this inherited behavior.
	The following code is only included for throw-away experimentation."
"
	aCanvas
		frameRectangle: (self localBounds insetBy: borderWidth)
		borderWidth: padding x
		color: `Color lightGreen alpha: 0.5`.
	aCanvas
		frameRectangle: (self localBounds insetBy: borderWidth)
		borderWidth: padding y
		color: `Color lightGreen alpha: 0.5`.
"! !

