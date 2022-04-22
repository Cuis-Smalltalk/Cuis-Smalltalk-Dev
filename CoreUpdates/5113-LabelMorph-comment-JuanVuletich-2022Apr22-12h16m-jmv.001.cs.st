'From Cuis 6.0 [latest update: #5112] on 22 April 2022 at 12:16:44 pm'!

!LabelMorph commentStamp: '<historical>' prior: 0!
LabelMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextModelMorph or TextParagraphMorph.

Structure:
instance var    	Type              Description 
font 			StrikeFont 		(normally nil; then the accessor #font gives back  a Font or nil #defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 	italics, struckThrough) 
contents 		String 			The text that will be displayed. 
!

