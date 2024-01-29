'From Cuis6.3 [latest update: #6179] on 8 January 2024 at 5:22:17 pm'!

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 1/8/2024 17:18:05'!
< aCharacter 
	"Compare with the standard case sensitive collation order.
	This stuff should be in Character and not in String..."

	self == aCharacter ifTrue: [ ^false ].

	(self codePoint < 256 and: [ aCharacter codePoint < 256 ]) ifTrue: [
		^String does: self collateBefore: aCharacter ].
	
	^self codePoint < aCharacter codePoint.! !

