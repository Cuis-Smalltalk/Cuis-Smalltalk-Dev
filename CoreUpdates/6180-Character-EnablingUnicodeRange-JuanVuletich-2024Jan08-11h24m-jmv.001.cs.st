'From Cuis6.3 [latest update: #6171] on 8 January 2024 at 11:47:38 am'!

!Character methodsFor: 'comparing' stamp: 'jmv 1/8/2024 11:30:16'!
< aCharacter 
	"Compare with the standard case sensitive collation order.
	This stuff should be in Character and not in String..."

	self == aCharacter ifTrue: [ ^false ].

	(self codePoint < 256 and: [ aCharacter codePoint < 256 ]) ifTrue: [
		^String does: self collateBefore: aCharacter ].
	
	^self codePoint < aCharacter codePoint.! !

!Character methodsFor: 'comparing' stamp: 'jmv 1/8/2024 11:38:34'!
<= aCharacter 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aCharacter) not.! !

!Character methodsFor: 'comparing' stamp: 'jmv 1/8/2024 11:39:12'!
= aCodePointOrCharacter

	self == aCodePointOrCharacter ifTrue: [ ^true ].

	"Just remove this method and use inherited version when we remove UnicodeCodePoint ?"
	aCodePointOrCharacter class == UnicodeCodePoint ifFalse: [ ^false ].

	aCodePointOrCharacter isCharacter ifFalse: [ ^false ].
	^self codePoint = aCodePointOrCharacter codePoint! !

!Character methodsFor: 'comparing' stamp: 'jmv 1/8/2024 11:31:16'!
> aCharacter 
	"Compare with the standard case sensitive collation order.
	This stuff should be in Character and not in String..."

	self == aCharacter ifTrue: [ ^false ].

	(self codePoint < 256 and: [ aCharacter codePoint < 256 ]) ifTrue: [
		^String does: self collateAfter: aCharacter ].
	
	^self codePoint > aCharacter codePoint.! !

!Character methodsFor: 'comparing' stamp: 'jmv 1/8/2024 11:39:54'!
>= aCharacter 
	"Answer whether the receiver is greater than or equal to the argument."

	^(self < aCharacter) not.! !

