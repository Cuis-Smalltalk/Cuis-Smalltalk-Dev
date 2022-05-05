'From Cuis 6.0 [latest update: #5135] on 5 May 2022 at 9:32:49 am'!

!Character class methodsFor: 'converting' stamp: 'jmv 5/5/2022 09:31:43'!
utf8BytesAndCodePointAt: byteIndex in: utf8ByteArray into: bytesBlock into: codePointBlock
	"Find next UTF-8 encoded code point in utf8ByteArray starting at byte byteIndex.
	Evalueate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence, skip it. Also skip codePoint U+FEFF (BOM, unneededly added by Win clipboard).
	But in any case, answer the number of bytes of the invalid sequence, to allow senders to update an index in an iteration.
	See senders."

	| n byte1 byte2 byte3 byte4 codePoint |
	n _ 0.
	byte1 _ utf8ByteArray at: byteIndex + n.
	n _ n + 1.
	byte1 < 128 ifTrue: [	"single byte"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: nil value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: byte1 ].
		^ n ].
	
	"At least 2 bytes"
	byte2 _ utf8ByteArray at: byteIndex + n.
	n _ n + 1.
	(byte2 bitAnd: 16rC0) = 16r80 ifFalse: [ ^n]. "invalid UTF-8"
	(byte1 bitAnd: 16rE0) = 192 ifTrue: [ "two bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePoint _ ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63).
			codePointBlock value: codePoint ].
		^n ].
	
	"At least 3 bytes"
	byte3 _ utf8ByteArray at: byteIndex + n.
	n _ n + 1.
	(byte3 bitAnd: 16rC0) = 16r80 ifFalse: [ ^n]. "invalid UTF-8"
	(byte1 bitAnd: 16rF0) = 224 ifTrue: [ "three bytes"
		codePoint _  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
		codePoint = 16rFEFF ifTrue: [ ^n ].
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: codePoint ].
		^n ].

	"4 bytes"
	byte4 _ utf8ByteArray at: byteIndex + n.
	n _ n + 1.
	(byte4 bitAnd: 16rC0) = 16r80 ifFalse: [ ^n]. "invalid UTF-8"
	(byte1 bitAnd: 16rF8) = 240 ifTrue: [  "four bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: byte4 ].
		codePointBlock ifNotNil: [
			codePoint _ ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12)
					+ ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63).
			codePointBlock value: codePoint ].
		].
	^n! !


!Character class methodsFor: 'converting' stamp: 'jmv 5/5/2022 09:32:15'!
nextCodePointBytesFromUtf8: anUtf8Stream into: aBlock
	"anUtf8Stream can be over a ByteArray.
	See senders."
"Note: This method will be removed. Prefer
#utf8BytesAndCodePointAt:in:into:into:
"
	| byte1 byte2 byte3 byte4 |
	byte1 _ anUtf8Stream next.
	byte1 < 128 ifTrue: [	"single byte"
		^ aBlock value: byte1 value: nil value: nil value: nil ].
	
	"At least 2 bytes"
	byte2 _ anUtf8Stream next.
	(byte2 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rE0) = 192 ifTrue: [ "two bytes"
		^ aBlock value: byte1 value: byte2 value: nil value: nil ].
	
	"At least 3 bytes"
	byte3 _ anUtf8Stream next.
	(byte3 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF0) = 224 ifTrue: [ "three bytes"
		^ aBlock value: byte1 value: byte2 value: byte3 value: nil ].

	"4 bytes"
	byte4 _ anUtf8Stream next.
	(byte4 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF8) = 240 ifTrue: [  "four bytes"
		^ aBlock value: byte1 value: byte2 value: byte3 value: byte4 ].

	^nil! !

!Character class methodsFor: 'converting' stamp: 'jmv 5/5/2022 09:32:19'!
nextUnicodeCodePointFromUtf8: anUtf8Stream
	"anUtf8Stream can be over a ByteArray
	Answer nil if conversion not possible, because of invalid UTF-8.
	Also answer nil for codePoint U+FEFF (BOM, unneededly added by Win clipboard)"
"Note: This method will be removed. Prefer
#utf8BytesAndCodePointAt:in:into:into:
"
	^ self
		nextCodePointBytesFromUtf8: anUtf8Stream
		into: [ :byte1 :byte2 :byte3 :byte4 |
			byte4 notNil
				ifTrue: [ ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12) + ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63) ]
				ifFalse: [
					byte3 notNil
						ifTrue: [ | codePoint |
							codePoint _  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
							codePoint = 16rFEFF
								ifFalse: [ codePoint ]
								ifTrue: [ nil ]]
						ifFalse: [
							byte2 notNil
								ifTrue: [ ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63) ]
								ifFalse: [ byte1 ]]]]! !

!Character class methodsFor: 'converting' stamp: 'jmv 5/5/2022 09:31:56'!
unicodeCodePointOfUtf8Bytes: aByteArray

	Character utf8BytesAndCodePointAt: 1 in: aByteArray
				into: nil
				into: [ :codePoint | ^ codePoint ].! !


!String class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 09:32:01'!
fromUtf8: aByteArray hex: useHexForNCRs trimLastNull: doTrimLastNullChar
	"Convert the given string from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8:
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

!String class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 09:32:05'!
utf32FromUtf8: aByteArray
	"Convert the given string from UTF-8 to UTF-32"

	^WordArray streamContents: [ :strm | | s byteIndex n |
		s _ aByteArray size.
		byteIndex _ 1.
		[  byteIndex <= s ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: aByteArray
				into: nil
				into: [ :codePoint |
					strm nextPut: codePoint ].
			byteIndex _ byteIndex + n
		]].! !

!methodRemoval: Character class #utfBytesAndCodePointAt:in:into:into: stamp: 'jmv 5/5/2022 09:32:28'!
Character class removeSelector: #utfBytesAndCodePointAt:in:into:into:!
