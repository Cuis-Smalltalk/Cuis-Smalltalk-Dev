'From Cuis 4.2 of 25 July 2013 [latest update: #1808] on 28 August 2013 at 12:29:09.970555 pm'!

!String class methodsFor: 'instance creation' stamp: 'jmv 8/28/2013 12:07'!
addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs
	"Convert the given Unicode codePoint to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	(Character unicodeCodePoint: codePoint)
		ifNotNil: [ :char | strm nextPut: char]
		ifNil: [
			useHexForNCRs
				ifTrue: [
					strm nextPutAll: '&#x'.
					codePoint printOn: strm base: 16 length: 4 padded: true.
					strm nextPut: $; ]
				ifFalse: [
					strm nextPutAll: '&#'.
					codePoint printOn: strm base: 10.
					strm nextPut: $; ]]! !


!PositionableStream methodsFor: 'testing' stamp: 'jmv 8/28/2013 11:43'!
isCharacters
	"Return true if the receiver is a Character stream"
	^collection class == String! !


!String class methodsFor: 'instance creation' stamp: 'jmv 8/28/2013 12:09'!
fromUtf8: aByteArray hex: useHexForNCRs trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^String streamContents: [ :strm | | bytes codePoint |
		bytes _ aByteArray readStream.
		[ bytes atEnd ] whileFalse: [
			codePoint _ (Character nextUnicodeCodePointFromUtf8: bytes).
			codePoint ifNotNil: [
				"Add just one char if possible, or the apropriate sequence for the NCR"
				self addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs ]].
			(doTrimLastNullChar and: [ codePoint = 0 ]) ifTrue: [
				strm position: strm position - 1 ]]! !

