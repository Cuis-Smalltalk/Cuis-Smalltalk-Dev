'From Cuis7.1 [latest update: #6697] on 11 September 2024 at 4:06:12 pm'!
!classDefinition: #ColoredBoxMorph category: #'Morphic-Kernel'!
BoxMorph subclass: #ColoredBoxMorph
	instanceVariableNames: 'color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!ColoredBoxMorph commentStamp: '<historical>' prior: 0!
ColoredRectMorph adds a customizable background color.!


!ColoredBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 16:01:35'!
color

	^ color! !

!ColoredBoxMorph methodsFor: 'accessing' stamp: 'jmv 9/11/2024 16:01:35'!
color: aColor
	"Set the receiver's color. "
	color = aColor ifFalse: [
		color := aColor.
		self redrawNeeded ]! !

!ColoredBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/11/2024 16:05:24'!
defaultColor
	^ `Color lightRed`! !

!ColoredBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/11/2024 16:02:03'!
initialize
	super initialize.
	color := self defaultColor.! !

!ColoredBoxMorph methodsFor: 'drawing' stamp: 'jmv 9/11/2024 16:03:04'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."

	aCanvas
		fillRectangle: self localBounds
		color: color.! !

!ColoredBoxMorph methodsFor: 'testing' stamp: 'jmv 9/11/2024 16:03:21'!
is: aSymbol
	^ aSymbol == #ColoredBoxMorph or: [ super is: aSymbol ]! !

!ColoredBoxMorph reorganize!
('accessing' color color:)
('initialization' defaultColor initialize)
('drawing' drawOn:)
('testing' is:)
!

