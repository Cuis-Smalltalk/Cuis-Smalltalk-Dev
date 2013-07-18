'From Cuis 4.1 of 12 December 2012 [latest update: #1582] on 28 January 2013 at 11:56:36 pm'!
!classDefinition: #StringMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #StringMorph
	instanceVariableNames: 'font emphasis contents hasFocus '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!

!StringMorph commentStamp: '<historical>' prior: 0!
StringMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextMorph.

Structure:
instance var    	Type              Description 
font 			StrikeFont 		(normally nil; then the accessor #font gives back  a Font or nil #defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 	italics, struckThrough) 
contents 		String 			The text that will be displayed. 
!


!StringMorph methodsFor: 'initialization' stamp: 'jmv 1/28/2013 23:55'!
initialize
	super initialize.
	font _ nil.
	emphasis _ 0.
	self contents: 'String Morph'
! !

!classDefinition: #StringMorph category: #'Morphic-Basic'!
RectangleLikeMorph subclass: #StringMorph
	instanceVariableNames: 'font emphasis contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
