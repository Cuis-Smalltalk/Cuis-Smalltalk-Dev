'From Cuis 6.0 [latest update: #5357] on 6 July 2022 at 9:33:57 am'!

!Character methodsFor: 'comparing' stamp: 'jmv 7/6/2022 06:49:21'!
>= aCharacter 
	"Answer whether the receiver is greater than or equal to the argument."

	aCharacter isUnicodeCodePoint ifTrue: [ ^aCharacter <= self ].
	^(self < aCharacter) not! !


!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 7/6/2022 09:28:10'!
< aCodePointOrCharacter
	self asCharacter ifNotNil: [ :selfAsChar |
		aCodePointOrCharacter asCharacter ifNotNil: [ :aChar |
			^selfAsChar < aChar ].
		^true ].
	aCodePointOrCharacter asCharacter ifNotNil: [ :aChar |
		^false ].
	^self codePoint < aCodePointOrCharacter codePoint! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 7/6/2022 09:29:22'!
<= aCodePointOrCharacter
	self = aCodePointOrCharacter ifTrue: [ ^true ].
	^ self < aCodePointOrCharacter! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 7/6/2022 09:30:37'!
> aCodePointOrCharacter
	self = aCodePointOrCharacter ifTrue: [ ^false ].
	^ (self < aCodePointOrCharacter) not! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 7/6/2022 09:29:50'!
>= aCodePointOrCharacter
	^ (self < aCodePointOrCharacter) not! !

