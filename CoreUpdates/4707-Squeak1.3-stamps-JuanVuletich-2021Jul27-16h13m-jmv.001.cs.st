'From Cuis 5.0 [latest update: #4701] on 27 July 2021 at 4:15:04 pm'!

!TextLine methodsFor: 'accessing' stamp: 'Squeak1.3 1/16/1998 17:55:27'!
baseline
	^ baseline! !

!TextLine methodsFor: 'accessing' stamp: 'Squeak1.3 1/16/1998 17:55:27'!
internalSpaces
	"Answer the number of spaces in the line."

	^internalSpaces! !

!TextLine methodsFor: 'accessing' stamp: 'Squeak1.3 1/16/1998 17:55:27'!
internalSpaces: spacesInteger 
	"Set the number of spaces in the line to be spacesInteger."

	internalSpaces _ spacesInteger! !

!TextLine methodsFor: 'accessing' stamp: 'Squeak1.3 1/16/1998 17:55:27'!
paddingWidth
	"Answer the amount of space to be added to the font."

	^paddingWidth! !

!TextLine methodsFor: 'accessing' stamp: 'Squeak1.3 1/16/1998 17:55:27'!
paddingWidth: padWidthInteger 
	"Set the amount of space to be added to the font to be padWidthInteger."

	paddingWidth _ padWidthInteger! !

!TextLine methodsFor: 'scanning' stamp: 'Squeak1.3 1/16/1998 17:55:27'!
justifiedPadFor: spaceIndex 
	"Compute the width of pad for a given space in a line of justified text."

	| pad |
	internalSpaces = 0 ifTrue: [^0].
	pad _ paddingWidth // internalSpaces.
	spaceIndex <= (paddingWidth \\ internalSpaces)
		ifTrue: [^pad + 1]
		ifFalse: [^pad]! !

!TextLine methodsFor: 'private' stamp: 'Squeak1.3 1/16/1998 17:55:27'!
internalSpaces: spacesInteger paddingWidth: padWidthInteger

	internalSpaces _ spacesInteger.
	paddingWidth _ padWidthInteger! !

