'From Cuis7.1 [latest update: #6697] on 11 September 2024 at 4:09:21 pm'!
!classDefinition: #BorderedBoxMorph category: #'Morphic-Kernel'!
ColoredBoxMorph subclass: #BorderedBoxMorph
	instanceVariableNames: 'borderWidth borderColor padding'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!BorderedBoxMorph methodsFor: 'initialization' stamp: 'jmv 9/11/2024 16:08:34'!
initialize
	super initialize.
	borderColor := self defaultBorderColor.
	borderWidth := self defaultBorderWidth.
	padding := self defaultPadding.! !

!methodRemoval: BorderedBoxMorph #color stamp: 'jmv 9/11/2024 16:08:01'!
BorderedBoxMorph removeSelector: #color!
!methodRemoval: BorderedBoxMorph #color: stamp: 'jmv 9/11/2024 16:08:13'!
BorderedBoxMorph removeSelector: #color:!
