'From Cuis 4.1 of 12 December 2012 [latest update: #1598] on 14 February 2013 at 2:38:49 pm'!

!String class methodsFor: 'instance creation' stamp: 'jmv 2/14/2013 14:29'!
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
								strm nextPut: $; ]]]].
			(doTrimLastNullChar and: [ codePoint = 0 ]) ifTrue: [
				strm position: strm position - 1 ].
		] ! !


!Character class methodsFor: 'converting' stamp: 'jmv 2/14/2013 14:32'!
nextUnicodeCodePointFromUtf8: anUtf8Stream
	"anUtf8Stream can be over a ByteArray
	Answer nil if conversion not possible, because of invalid UTF-8.
	Also answer nil for codePoint U+FEFF (BOM, unneededly added by Win clipboard)"

	| byte1 byte2 byte3 byte4 codePoint |
	byte1 _ anUtf8Stream next asInteger.
	byte1 < 128 ifTrue: [	"single byte"
		^byte1 ].
	
	"At least 2 bytes"
	byte2 _ anUtf8Stream next asInteger.
	(byte2 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rE0) = 192 ifTrue: [ "two bytes"
		^ ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63) ].
	
	"At least 3 bytes"
	byte3 _ anUtf8Stream next asInteger.
	(byte3 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF0) = 224 ifTrue: [ "three bytes"
		codePoint _  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
		^codePoint = 16rFEFF
			ifFalse: [ codePoint ]
			ifTrue: [ nil ]].

	"4 bytes"
	byte4 _ anUtf8Stream next asInteger.
	(byte4 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF8) = 240 ifTrue: [  "four bytes"
		^((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12) + ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63) ].

	^nil! !


!Clipboard methodsFor: 'private' stamp: 'jmv 2/14/2013 14:30'!
retrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| primitiveFormat interface |

	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface |
		interface _ clipboardInterface current.
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ clipboardInterface current retrieveId ]].
		
	primitiveFormat _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard.
	We prefer nil"
	primitiveFormat isEmpty ifTrue: [ ^nil ].

	"The VM uses UTF-8 for clipboard"
	^(String fromUtf8: primitiveFormat hex: false trimLastNull: true) withCuisLineEndings! !


!String class methodsFor: 'instance creation' stamp: 'jmv 2/14/2013 14:29'!
fromUtf8: aByteArray
	"Convert the given bytes from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf8 "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	^self fromUtf8: aByteArray hex: false trimLastNull: false! !


!StringTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/14/2013 14:30'!
testAsUtf8WithNCRs
	"
	 StringTest new testAsUtf8WithNCRs
	"
	| stringWithDecimalNCRs stringWithHexNCRs utf8 |
	utf8 _ ByteArray readHexFrom: ('CE BC 20 CE A9 20 CF 89 20 54 68 65 20 64 65 63 6F 6D 70 6F 73 69 74 69 6F 6E 20 6D 61 70 70 69 6E 67 20 69 73 20 3C EC B8 A0 2C 20 E1 86 B8 3E 2C 20 61 6E 64 20 6E 6F 74 20 3C E1 84 8E 2C 20 E1 85 B3 2C 20 31 31 42 38 3E 2E 0A 3C 70 3E 54 68 65 20 74 69 74 6C 65 20 73 61 79 73 20 E2 80 AB D7 A4 D7 A2 D7 99 D7 9C D7 95 D7 AA 20 D7 94 D7 91 D7 99 D7 A0 D7 90 D7 95 D7 9D 2C 20 57 33 43 E2 80 AC 20 69 6E 20 48 65 62 72 65 77 3C 2F 70 3E 0A 61 62 63 E0 A4 95 E0 A4 96 E0 A5 80 E5 9C 8B E9 9A 9B F0 90 8E 84 F0 90 8E 94 F0 90 8E 98' reject: [ :char  | char isSeparator ]).

	stringWithDecimalNCRs _ String fromUtf8: utf8 hex: false trimLastNull: false.
	stringWithHexNCRs _ String fromUtf8: utf8 hex: true trimLastNull: false.

	self assert: stringWithDecimalNCRs = '&#956; &#937; &#969; The decomposition mapping is <&#52768;, &#4536;>, and not <&#4366;, &#4467;, 11B8>.
<p>The title says &#8235;&#1508;&#1506;&#1497;&#1500;&#1493;&#1514; &#1492;&#1489;&#1497;&#1504;&#1488;&#1493;&#1501;, W3C&#8236; in Hebrew</p>
abc&#2325;&#2326;&#2368;&#22283;&#38555;&#66436;&#66452;&#66456;'.

	self assert: (stringWithDecimalNCRs asUtf8: true) = utf8.

	self assert: stringWithHexNCRs = '&#x03BC; &#x03A9; &#x03C9; The decomposition mapping is <&#xCE20;, &#x11B8;>, and not <&#x110E;, &#x1173;, 11B8>.
<p>The title says &#x202B;&#x05E4;&#x05E2;&#x05D9;&#x05DC;&#x05D5;&#x05EA; &#x05D4;&#x05D1;&#x05D9;&#x05E0;&#x05D0;&#x05D5;&#x05DD;, W3C&#x202C; in Hebrew</p>
abc&#x0915;&#x0916;&#x0940;&#x570B;&#x969B;&#x10384;&#x10394;&#x10398;'.

	self assert: (stringWithHexNCRs asUtf8: true) = utf8! !

!methodRemoval: String class #fromUtf8:hex:!
String class removeSelector: #fromUtf8:hex:!
