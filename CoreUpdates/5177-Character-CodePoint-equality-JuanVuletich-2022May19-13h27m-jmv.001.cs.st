'From Cuis 6.0 [latest update: #5174] on 19 May 2022 at 1:30:55 pm'!

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 5/17/2022 16:39:47'!
= aCodePointOrCharacter

	self == aCodePointOrCharacter ifTrue: [ ^true ].
	aCodePointOrCharacter isCharacterLike ifFalse: [ ^false ].
	^self codePoint = aCodePointOrCharacter codePoint! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 5/19/2022 13:30:01'!
hash
	"Hash is reimplemented because = is implemented."

	^value hash! !


!Character methodsFor: 'comparing' stamp: 'jmv 5/17/2022 16:39:43'!
= aCodePointOrCharacter

	self == aCodePointOrCharacter ifTrue: [ ^true ].
	aCodePointOrCharacter isCharacterLike ifFalse: [ ^false ].
	^self codePoint = aCodePointOrCharacter codePoint! !

