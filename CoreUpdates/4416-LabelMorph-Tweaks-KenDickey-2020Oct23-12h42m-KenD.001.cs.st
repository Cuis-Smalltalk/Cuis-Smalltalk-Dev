'From Cuis 5.0 [latest update: #4415] on 23 October 2020 at 12:44:10 pm'!

!LabelMorph methodsFor: 'font' stamp: 'KenD 10/23/2020 08:09:09'!
emphasis: aNumber
	"Set the receiver's emphasis as indicated. aNumber is a bitmask with the following format:

	bit	attribute
	1	bold
	2	italic
	4	underlined
	8	struckThrough
	16	withUnderscoreGlyphs
	"

	"examples: 0 -> plain.  
	1 -> bold.  2 -> italic.  3 -> bold italic.  4 -> underlined  
	5 -> bold underlined.  6 -> italic underlined.   7 -> bold italic underlined   
	etc...
	
	Prefer AbstractFont method category: 'derivatives - emphasis' 
	to raw numbers:
	    self emphasis: AbstractFont boldItalic.
	etc."

	emphasis _ aNumber.
	^ self font: font emphasis: emphasis! !

!LabelMorph methodsFor: 'geometry' stamp: 'KenD 10/16/2020 14:43:51'!
minimumExtent

	^ self measureContents + (2 * self borderWidth)! !

