'From Cuis 5.0 of 7 November 2016 [latest update: #3285] on 4 April 2018 at 3:16:11 pm'!

!AbstractFont methodsFor: 'measuring' stamp: 'jmv 4/4/2018 14:46:40'!
normalizedWidthOf: aCharacter
	"Return the width of the given character, irrespective of point size."
	^ (self widthOf: aCharacter) / self pointSize! !

