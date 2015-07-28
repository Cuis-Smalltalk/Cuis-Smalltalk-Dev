'From Cuis 4.2 of 25 July 2013 [latest update: #2424] on 20 July 2015 at 10:33:39.929738 pm'!

!Object methodsFor: 'printing' stamp: 'KenD 7/20/2015 22:33'!
printStringLimitedTo: limit
	"Answer a String whose characters are a description of the receiver.
	If you want to print without a character limit, use fullPrintString."
	| limitedString |
	limitedString _ String streamContents: [:s | self printOn: s] limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString , '[..]'! !

