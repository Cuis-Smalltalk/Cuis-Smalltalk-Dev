'From Cuis 6.0 [latest update: #5187] on 23 May 2022 at 10:26:49 am'!

!UnicodeCodePoint methodsFor: 'accessing' stamp: 'jmv 5/23/2022 10:18:26'!
asciiValue
	"Answer the value of the receiver that represents its ANSI encoding.
	This is the same as the Unicode CodePoint for the first 128 characters.
	Answer nil if out of range."

	^ value < 128 ifTrue: [ value ].! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 5/23/2022 10:06:19'!
printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean
	"Print as per ISO 8601 section 5.4.2
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c codePoint > `$0 codePoint` ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		aStream
			nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
			nextPutAll: (offset hours abs printString padded: #left to: 2 with: $0);
			nextPut: $:;
			nextPutAll: (offset minutes abs printString padded: #left to: 2 with: $0).
		"Note: We should check the ISO to see if the seconds part is supported by the standard
		(usually not an issue. offsets usually don't include seconds)"
		offset seconds = 0 ifFalse: [
			aStream
				nextPut: $:;
				nextPutAll: (offset seconds abs truncated printString) ]]! !


!Character methodsFor: 'accessing' stamp: 'jmv 5/23/2022 10:17:45'!
asciiValue
	"Answer the value of the receiver that represents its ANSI encoding.
	This is the same as ISO 8859-15 for the first 128 characters.
	Answer nil if out of range."

	| n |
	n _ self numericValue.
	^ n < 128 ifTrue: [ n ].! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 5/23/2022 10:11:27'!
add: aCharacter
	map at: aCharacter iso8859s15Code+1  put: 1.! !

!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 5/23/2022 10:11:42'!
remove: aCharacter
	map at: aCharacter iso8859s15Code + 1  put: 0! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/23/2022 10:15:05'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ `$_ numericValue` = value ]! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/23/2022 10:15:10'!
isValidStartOfIdentifiers
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	^self isLetter or: [ `$_ numericValue` = value ]! !

!UnicodeCodePoint methodsFor: 'comparing' stamp: 'jmv 5/23/2022 10:09:12'!
hash
	"Hash is reimplemented because = is implemented."

	self iso8859s15Code ifNotNil: [ :sameNumericValueAsCharacter | ^sameNumericValueAsCharacter hash ].
	^value hash! !

