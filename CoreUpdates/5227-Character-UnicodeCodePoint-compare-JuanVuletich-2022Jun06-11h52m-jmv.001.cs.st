'From Cuis 6.0 [latest update: #5226] on 6 June 2022 at 11:54:07 am'!

!Character methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:36'!
< aCharacter 
	"Compare with the standard case sensitive collation order.
	This stuff should be in Character and not in String..."

	aCharacter isUnicodeCodePoint ifTrue: [ ^aCharacter > self ].
	^String does: self collateBefore: aCharacter! !

!Character methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:40'!
<= aCharacter 
	"Answer whether the receiver is less than or equal to the argument."

	aCharacter isUnicodeCodePoint ifTrue: [ ^aCharacter >= self ].
	^(self > aCharacter) not! !

!Character methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:44'!
> aCharacter 
	"Compare with the standard case sensitive collation order.
	This stuff should be in Character and not in String..."

	aCharacter isUnicodeCodePoint ifTrue: [ ^aCharacter < self ].
	^String does: self collateAfter: aCharacter! !

!Character methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:48'!
>= aCharacter 
	"Answer whether the receiver is greater than or equal to the argument."

	aCharacter isUnicodeCodePoint ifTrue: [ ^aCharacter <= self ].
	^aCharacter <= self! !


!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:16'!
< aCodePointOrCharacter

	^self codePoint < aCodePointOrCharacter codePoint! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:20'!
<= aCodePointOrCharacter

	^self codePoint <= aCodePointOrCharacter codePoint! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:25'!
> aCodePointOrCharacter

	^self codePoint > aCodePointOrCharacter codePoint! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 6/6/2022 11:53:29'!
>= aCodePointOrCharacter

	^self codePoint >= aCodePointOrCharacter codePoint! !

