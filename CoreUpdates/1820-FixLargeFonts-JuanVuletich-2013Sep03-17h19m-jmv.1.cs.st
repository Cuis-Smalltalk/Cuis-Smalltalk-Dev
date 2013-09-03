'From Cuis 4.2 of 25 July 2013 [latest update: #1819] on 3 September 2013 at 5:19:26.667323 pm'!

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 9/3/2013 17:17'!
widthOf: aCharacter 
	"Answer the width of the argument as a character in the receiver."

	| ascii |
	ascii _ characterToGlyphMap
		ifNil: [ aCharacter asciiValue ]
		ifNotNil: [ characterToGlyphMap at: aCharacter asciiValue + 1 ].
	(ascii >= minAscii and:[ascii <= maxAscii]) ifFalse: [ascii _ maxAscii + 1].
	^ (xTable at: ascii + 2) - (xTable at: ascii + 1) - self baseKern! !

