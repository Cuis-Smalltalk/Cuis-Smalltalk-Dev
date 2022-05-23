'From Cuis 6.0 [latest update: #5183] on 23 May 2022 at 9:54:03 am'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/23/2022 09:49:47'!
iso8859s15Code
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding.
	Any implementation of Character like object beyond ISO-8859-15 should answer nil.
	Senders, please consider that this method might answer nil.
	See #iso8859s15CodeForUnicodeCodePoint:"

	value < 128 ifTrue: [ ^value ]."Plain ASCII"
	self asCharacter ifNotNil: [ :ch | ^ch iso8859s15Code ].
	^nil! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 5/23/2022 09:49:32'!
asCharacter
	"Might answer nil"
	^Character codePoint: value! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 5/23/2022 09:53:24'!
includes: aCharacter
	aCharacter isCharacterLike ifFalse: [ ^ false ].
	^aCharacter iso8859s15Code ifNotNil: [ :index | (map at: index + 1) > 0 ].! !


!String methodsFor: 'accessing' stamp: 'jmv 5/23/2022 09:52:13'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacterLike
		ifTrue: [	
			(aCharacter is: #Character)
				ifFalse: [
					aCharacter asCharacter
						ifNotNil: [ :ch | ^self at: index put: ch ]
						ifNil: [self error: 'String only store code points in the ISO 8859-15 set']]
				ifTrue: [
					index isInteger
						ifTrue: [self errorSubscriptBounds: index]
						ifFalse: [self errorNonIntegerIndex]]]
		ifFalse: [self error: 'Strings only store Characters']! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/23/2022 09:49:40'!
isLetter
	"Answer whether the receiver is a letter."
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^ch isLetter ].
	^false! !


!UnicodeCodePoint reorganize!
('accessing' codePoint codePoint:)
('testing' is: isAlphaNumeric isCharacterLike isDigit isLetter isLineSeparator isSeparator isValidInIdentifiers isValidStartOfIdentifiers iso8859s15Code)
('as yet unclassified' doesNotUnderstand:)
('comparing' = hash)
('converting' asCharacter)
!

