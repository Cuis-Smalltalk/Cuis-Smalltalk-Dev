'From Cuis 6.0 [latest update: #5308] on 23 June 2022 at 5:04:24 pm'!

!Character methodsFor: 'converting' stamp: 'jmv 6/23/2022 17:01:52'!
asUnicodeCodePoint
	^UnicodeCodePoint codePoint: self codePoint! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/23/2022 16:49:59'!
isUnaccented
	"Answer whether the receiver is an unaccented letter."
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^ch isUnaccented ].
	^false! !

!UnicodeCodePoint methodsFor: 'copying' stamp: 'jmv 6/23/2022 16:54:34'!
shallowCopy
	"Answer with me, because we are immutable."! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 6/23/2022 16:43:47'!
asString
	^Utf8String with: self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 6/23/2022 17:02:06'!
to: other
	"Answer with a collection in ascii order
	$a asUnicodeCodePoint to: $z asUnicodeCodePoint
	"
	^ (self codePoint to: other codePoint) collect:
				[ :codePoint | UnicodeCodePoint codePoint: codePoint ]! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 6/23/2022 17:04:07'!
withDiacriticalMark: anUnicodeCodePoint
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch |
		^UnicodeCodePoint codePoint: (ch withDiacriticalMark: anUnicodeCodePoint) codePoint ].
	^self! !

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 6/23/2022 16:47:17'!
hex
	^value printStringBase: 16 length: 6 padded: true! !

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 6/23/2022 16:49:08'!
isLiteral
self flag: #unicodeNeedsImprovement.
	^super isLiteral! !

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 6/23/2022 16:51:57'!
printOn: aStream 
	aStream nextPut: $$; nextPut: $(.
	aStream nextPutAll: self hex.
	aStream nextPut: $).! !

!UnicodeCodePoint methodsFor: 'printing' stamp: 'jmv 6/23/2022 16:59:07'!
storeOn: aStream

	aStream nextPutAll: '(UnicodeCodePoint codePoint: '.
	aStream print: value.
	aStream nextPut: $).! !

!UnicodeCodePoint methodsFor: 'private' stamp: 'jmv 6/23/2022 16:55:56'!
privateValue: aNumericCodePoint
	value _ aNumericCodePoint! !


!Utf8String methodsFor: 'copying' stamp: 'jmv 6/23/2022 16:53:55'!
shallowCopy
	"Answer with me, because we are immutable."! !


!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 6/23/2022 16:44:03'!
asSymbol
	^self asString asSymbol! !


!UnicodeCodePoint class methodsFor: 'instance creation' stamp: 'jmv 6/23/2022 16:56:24'!
codePoint: aNumber
	^self new privateValue: aNumber! !

!methodRemoval: UnicodeCodePoint #codePoint: stamp: 'jmv 6/23/2022 16:56:40'!
UnicodeCodePoint removeSelector: #codePoint:!
!methodRemoval: Character #leadingChar stamp: 'jmv 6/23/2022 16:50:18'!
Character removeSelector: #leadingChar!

!UnicodeCodePoint reorganize!
('accessing' asciiValue codePoint digitValue numericValue)
('testing' canBeGlobalVarInitial is: isAccented isAlphaNumeric isCharacter isDigit isLetter isLineSeparator isLowercase isSeparator isUnaccented isUnicodeCodePoint isUppercase isValidInBinarySelectors isValidInIdentifiers isValidStartOfIdentifiers isVowel iso8859s15Code tokenish)
('copying' shallowCopy)
('as yet unclassified' doesNotUnderstand:)
('comparing' < <= = > >= hash)
('converting' asCharacter asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('printing' hex isLiteral printOn: storeOn:)
('private' privateValue:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!

