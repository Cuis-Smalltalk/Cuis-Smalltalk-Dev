'From Cuis7.1 [latest update: #6680] on 9 September 2024 at 5:51:44 pm'!
!classDefinition: #BorderedBoxMorph category: #'Morphic-Kernel'!
BoxMorph subclass: #BorderedBoxMorph
	instanceVariableNames: 'color borderWidth borderColor padding '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'KenD 9/9/2024 14:02:49'!
padding
	^ padding! !

!BorderedBoxMorph methodsFor: 'accessing' stamp: 'KenD 9/9/2024 16:53:56'!
padding: numberOrPoint
	| p |
	p := numberOrPoint asPoint.
	padding = p ifFalse: [
		padding := p max: `0@0`.
		self ensureMinimumExtent;
			redrawNeeded ].! !

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'KenD 9/9/2024 16:59:41'!
defaultPadding
	"answer the default inner padding for the receiver"
	^ `10@6`! !

!BorderedBoxMorph methodsFor: 'geometry' stamp: 'KenD 9/9/2024 13:56:09'!
interiorExtent

	"Answer extent inset from borderWidth and Padding"
	^self morphExtent - (2 * borderWidth) - (2 * padding)! !

!BorderedBoxMorph methodsFor: 'geometry' stamp: 'KenD 9/9/2024 13:57:58'!
interiorOrigin

	"Answer point from borderWidth and Padding"
	| x y |
	x := borderWidth + padding x.
	y := borderWidth + padding y.
	^x @ y! !

!BorderedBoxMorph methodsFor: 'geometry' stamp: 'jmv 9/9/2024 17:51:06'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ super minimumExtent + (2 * self interiorOrigin)! !


!BorderedBoxMorph methodsFor: 'accessing' stamp: 'KenD 9/9/2024 16:54:12'!
borderWidth: anInteger
	borderWidth = anInteger ifFalse: [
		borderWidth := anInteger max: 0.
		self ensureMinimumExtent;
			redrawNeeded ].! !

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/9/2024 13:30:18'!
initialize
	super initialize.
	color := self defaultColor.
	borderColor := self defaultBorderColor.
	borderWidth := self defaultBorderWidth.
	padding := self defaultPadding.! !

!BorderedBoxMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2024 13:43:30'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor.

	"Make the existence of the normal invisible padding evident."
	aCanvas
		frameRectangle: (self localBounds insetBy: borderWidth)
		borderWidth: padding x
		color: `Color lightGreen alpha: 0.5`.
	aCanvas
		frameRectangle: (self localBounds insetBy: borderWidth)
		borderWidth: padding y
		color: `Color lightGreen alpha: 0.5`.! !

!classDefinition: #BorderedBoxMorph category: #'Morphic-Kernel'!
BoxMorph subclass: #BorderedBoxMorph
	instanceVariableNames: 'color borderWidth borderColor padding'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
