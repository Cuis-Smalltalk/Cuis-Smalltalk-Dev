'From Cuis 6.0 [latest update: #5328] on 28 June 2022 at 2:54:52 pm'!

!String methodsFor: 'converting' stamp: 'jmv 6/28/2022 14:45:48'!
translateFrom: start to: stop table: table
	"translate the characters in the string by the given table, in place"
	String translate: self from: start to: stop table: table! !


!String class methodsFor: 'instance creation' stamp: 'jmv 6/28/2022 14:44:33'!
addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs
	"Convert the given Unicode codePoint to the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8Bytes:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	(Character codePoint: codePoint trySimilar: true)
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

!String class methodsFor: 'instance creation' stamp: 'jmv 6/28/2022 14:44:27'!
fromUtf32Words: aWordArray
	"Convert the given bytes from UTF-32 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf32Words "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	^self fromUtf32Words: aWordArray hex: false trimLastNull: false! !

!String class methodsFor: 'instance creation' stamp: 'jmv 6/28/2022 14:44:45'!
fromUtf32Words: aWordArray hex: useHexForNCRs trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-32 to the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf32Words:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^String streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			"Add just one char if possible, or the apropriate sequence for the NCR"
			self addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs ].
		(doTrimLastNullChar and: [ aWordArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !

!String class methodsFor: 'instance creation' stamp: 'jmv 6/28/2022 14:44:48'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf8Bytes "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding and possible NCR generation."
	^self fromUtf8Bytes: aByteArray hex: false trimLastNull: false! !

!String class methodsFor: 'instance creation' stamp: 'jmv 6/28/2022 14:44:52'!
fromUtf8Bytes: aByteArray hex: useHexForNCRs trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8Bytes:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^String streamContents: [ :strm | | s byteIndex n |
		s _ aByteArray size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					"Add just one char if possible, or the apropriate sequence for the NCR"
					self addUnicodeCodePoint: codePoint to: strm hex: useHexForNCRs ].
			byteIndex _ byteIndex + n
		].
		(doTrimLastNullChar and: [ aByteArray last = 0 ]) ifTrue: [
			strm position: strm position - 1 ]].! !

!String class methodsFor: 'primitives' stamp: 'jmv 6/28/2022 14:45:44'!
translate: aString from: start to: stop table: table
	"translate the characters in the string by the given table, in place"
	<primitive: 'primitiveTranslateStringWithTable' module: 'MiscPrimitivePlugin'>

	start to: stop do: [ :i |
		aString at: i put: (table at: (aString basicAt: i) +1) ]! !

!String class methodsFor: 'services' stamp: 'jmv 6/28/2022 11:48:16'!
compare: string1 with: string2
	"Return 1, 2 or 3, if string1 is <, =, or > string2,
	with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: CaseSensitiveOrder! !

!String class methodsFor: 'services' stamp: 'jmv 6/28/2022 14:44:38'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start. The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>

	key isEmpty ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index _ 1.
			[(matchTable at: (body basicAt: startIndex+index-1) + 1)
				= (matchTable at: (key basicAt: index) + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index _ index+1]].
	^ 0
"
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"! !


!Symbol methodsFor: 'accessing' stamp: 'jmv 6/28/2022 14:45:08'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise. The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self isEmpty ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c _ self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1! !

!Symbol methodsFor: 'system primitives' stamp: 'di 1/2/1999 17:00'!
flushCache
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>
! !


!Utf8Symbol methodsFor: 'accessing' stamp: 'jmv 6/28/2022 14:45:18'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise. The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	| c |
	self isEmpty ifTrue: [^ 0].
	"Consider selectors starting with an underscore $_ as unary, even if Preferences allowUnderscoreSelectors is not set."
	c _ self first.
	c isValidInBinarySelectors ifTrue: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1! !

!Utf8Symbol methodsFor: 'system primitives' stamp: 'di 1/2/1999 17:00'!
flushCache
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>
! !

