'From Cuis 4.1 of 12 December 2012 [latest update: #1588] on 8 February 2013 at 8:52:49 am'!

!Character class methodsFor: 'converting' stamp: 'jmv 2/8/2013 08:32'!
evaluate: aBlock withUtf8BytesOfUnicodeCodePoint: aCodePoint
	"See senders for typical usage"

	| mask nBytes shift |
	aCodePoint < 128 ifTrue: [
		^aBlock value: aCodePoint ].
	nBytes _ aCodePoint highBit + 3 // 5.
	mask _ #(128 192 224 240 248 252 254 255) at: nBytes.
	shift _ nBytes - 1 * -6.
	aBlock value: (aCodePoint bitShift: shift) + mask.
	2 to: nBytes do: [ :i | 
		shift _ shift + 6.
		aBlock value: ((aCodePoint bitShift: shift) bitAnd: 63) + 128 ]! !

!Character class methodsFor: 'converting' stamp: 'jmv 2/8/2013 08:33'!
nextUnicodeCodePointFromUtf8: anUtf8Stream
	"anUtf8Stream can be over a ByteArray
	Answer nil if conversion not possible, because of invalid UTF-8"

	| byte1 byte2 byte3 byte4 |
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
		^ ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63) ].

	"4 bytes"
	byte4 _ anUtf8Stream next asInteger.
	(byte4 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF8) = 240 ifTrue: [  "four bytes"
		^ ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12) + ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63) ].

	^nil! !

!Character class methodsFor: 'converting' stamp: 'jmv 2/8/2013 08:33'!
unicodeCodePointOfUtf8Bytes: aByteArray

	^ Character nextUnicodeCodePointFromUtf8: aByteArray readStream! !

!Character class methodsFor: 'converting' stamp: 'jmv 2/8/2013 08:34'!
utf8BytesOfUnicodeCodePoint: aCodePoint

	^ ByteArray streamContents: [ :strm |
		Character
			evaluate: [ :byte |
				strm nextPut: byte ]
			withUtf8BytesOfUnicodeCodePoint: aCodePoint ]! !


!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/8/2013 08:16'!
testFromUTF8
	"
	CharacterTest new testFromUTF8
	"
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: '24')) hex = '16r24'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'C2A2')) hex = '16rA2'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'E282AC')) hex = '16r20AC'.
	self assert: (Character unicodeCodePointOfUtf8Bytes: (ByteArray readHexFrom: 'F0A4ADA2')) hex = '16r24B62'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/8/2013 08:15'!
testSomeLatinCharsFromUTF8
	"
	CharacterTest new testSomeLatinCharsFromUTF8
	"
	| bytes string |
	bytes _ (ByteArray readHexFrom: 'C3A1C3A5C3A6C3B1C386C2A5C3BC') readStream.
	string _ String streamContents: [ :strm |
		[bytes atEnd ] whileFalse: [
			strm nextPut: (Character value: (Character nextUnicodeCodePointFromUtf8: bytes )) ]].
	self assert: string = 'áåæñÆ¥ü'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/8/2013 08:14'!
testSomeLatinCharsToUTF8
	"
	CharacterTest new testSomeLatinCharsToUTF8
	"
	| characters bytes |
	characters _ 'áåæñÆ¥ü' readStream.
	bytes _ ByteArray streamContents: [ :strm |
		[ characters atEnd ] whileFalse: [
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: characters next asciiValue ]].
	self assert: bytes hex = 'C3A1C3A5C3A6C3B1C386C2A5C3BC'! !

!CharacterTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/8/2013 08:17'!
testToUTF8
	"
	CharacterTest new testToUTF8
	"
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r0024) hex =  '24'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r00A2) hex =  'C2A2'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r20AC) hex = 'E282AC'.
	self assert: (Character utf8BytesOfUnicodeCodePoint: 16r024B62) hex = 'F0A4ADA2'! !


!String methodsFor: 'converting' stamp: 'jmv 2/8/2013 08:40'!
asUtf8
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	See #fromUtf8: "

	^self asUtf8: false! !

!String methodsFor: 'converting' stamp: 'jmv 2/8/2013 08:46'!
asUtf8: convertEmbeddedNCRs
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray.
	
	If convertEmbeddedNCRs, then convert embedded NCRs such as '&#956;' (decimal) or '&#x03BC;' (hex) to CodePoints.
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	
	
	Note: The conversion of NCRs is reversible. See #fromUtf8:hex:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^ByteArray streamContents: [ :outStream | | inStream nextChar prevPos maybeUnicodeNCR ncrSize codePoint |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			(convertEmbeddedNCRs and: [ nextChar = $& ])
				ifTrue: [
					prevPos _ inStream position.
					maybeUnicodeNCR _ inStream next: 9.
					maybeUnicodeNCR first = $# ifTrue: [
						ncrSize _ maybeUnicodeNCR indexOf: $;.
						ncrSize = 0
							ifFalse: [
								codePoint _ maybeUnicodeNCR second = $x
									ifTrue: [ ('16r', (maybeUnicodeNCR copyFrom: 3 to: ncrSize) asUppercase) asNumber ]
									ifFalse: [ (maybeUnicodeNCR copyFrom: 2 to: ncrSize) asNumber ]]
							ifTrue: [
								"Not an NCR after all. Just add the $& and continue from there"
								codePoint _ nextChar unicodeCodePoint ].
						Character
							evaluate: [ :byte | outStream nextPut: byte ]
							withUtf8BytesOfUnicodeCodePoint: codePoint.
						inStream position: prevPos + ncrSize ]]
				ifFalse: [
					codePoint _ nextChar unicodeCodePoint.
					Character
						evaluate: [ :byte | outStream nextPut: byte ]
						withUtf8BytesOfUnicodeCodePoint: codePoint ]]]! !


!String class methodsFor: 'instance creation' stamp: 'jmv 2/8/2013 08:41'!
fromUtf8: aByteArray
	"Convert the given bytes from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf8 "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	^self fromUtf8: aByteArray hex: false! !

!String class methodsFor: 'instance creation' stamp: 'jmv 2/8/2013 08:46'!
fromUtf8: aByteArray hex: useHex
	"Convert the given string from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15)"
	"For unicode chars not in ISO Latin 9 (ISO 8859-15), embed Decimal NCRs or Hexadecimal NCRs according to useHex.
	
	See http://en.wikipedia.org/wiki/Numeric_character_reference
	See http://rishida.net/tools/conversion/. Tests prepared there.
	
	Note: The conversion of NCRs is reversible. See #asUtf8:
	This allows handling the full Unicode in Cuis tools, that can only display the Latin alphabet, by editing the NCRs.
	The conversions can be done when reading / saving files, or when pasting from Clipboard and storing back on it."

	^String streamContents: [ :strm | | bytes |
		bytes _ aByteArray readStream.
		[ bytes atEnd ] whileFalse: [
			(Character nextUnicodeCodePointFromUtf8: bytes) ifNotNil: [ :codePoint | 
				(Character unicodeCodePoint: codePoint)
					ifNotNil: [ :iso8859m15code | strm nextPut: iso8859m15code]
					ifNil: [
						useHex
							ifTrue: [
								strm nextPutAll: '&#x'.
								codePoint printOn: strm base: 16 length: 4 padded: true.
								strm nextPut: $; ]
							ifFalse: [
								strm nextPutAll: '&#'.
								codePoint printOn: strm base: 10.
								strm nextPut: $; ]]]]] ! !


!StringTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/8/2013 08:17'!
testAsUtf8
	"
	 StringTest new testAsUtf8
	"
	self assert: 'A¢¤' asUtf8 hex = '41C2A2E282AC'! !

!StringTest methodsFor: 'UTF-8 conversion' stamp: 'jmv 2/8/2013 08:19'!
testAsUtf8WithNCRs
	"
	 StringTest new testAsUtf8WithNCRs
	"
	| stringWithDecimalNCRs stringWithHexNCRs utf8 |
	utf8 _ ByteArray readHexFrom: ('CE BC 20 CE A9 20 CF 89 20 54 68 65 20 64 65 63 6F 6D 70 6F 73 69 74 69 6F 6E 20 6D 61 70 70 69 6E 67 20 69 73 20 3C EC B8 A0 2C 20 E1 86 B8 3E 2C 20 61 6E 64 20 6E 6F 74 20 3C E1 84 8E 2C 20 E1 85 B3 2C 20 31 31 42 38 3E 2E 0A 3C 70 3E 54 68 65 20 74 69 74 6C 65 20 73 61 79 73 20 E2 80 AB D7 A4 D7 A2 D7 99 D7 9C D7 95 D7 AA 20 D7 94 D7 91 D7 99 D7 A0 D7 90 D7 95 D7 9D 2C 20 57 33 43 E2 80 AC 20 69 6E 20 48 65 62 72 65 77 3C 2F 70 3E 0A 61 62 63 E0 A4 95 E0 A4 96 E0 A5 80 E5 9C 8B E9 9A 9B F0 90 8E 84 F0 90 8E 94 F0 90 8E 98' reject: [ :char  | char isSeparator ]).

	stringWithDecimalNCRs _ String fromUtf8: utf8 hex: false.
	stringWithHexNCRs _ String fromUtf8: utf8 hex: true.

	self assert: stringWithDecimalNCRs = '&#956; &#937; &#969; The decomposition mapping is <&#52768;, &#4536;>, and not <&#4366;, &#4467;, 11B8>.
<p>The title says &#8235;&#1508;&#1506;&#1497;&#1500;&#1493;&#1514; &#1492;&#1489;&#1497;&#1504;&#1488;&#1493;&#1501;, W3C&#8236; in Hebrew</p>
abc&#2325;&#2326;&#2368;&#22283;&#38555;&#66436;&#66452;&#66456;'.

	self assert: (stringWithDecimalNCRs asUtf8: true) = utf8.

	self assert: stringWithHexNCRs = '&#x03BC; &#x03A9; &#x03C9; The decomposition mapping is <&#xCE20;, &#x11B8;>, and not <&#x110E;, &#x1173;, 11B8>.
<p>The title says &#x202B;&#x05E4;&#x05E2;&#x05D9;&#x05DC;&#x05D5;&#x05EA; &#x05D4;&#x05D1;&#x05D9;&#x05E0;&#x05D0;&#x05D5;&#x05DD;, W3C&#x202C; in Hebrew</p>
abc&#x0915;&#x0916;&#x0940;&#x570B;&#x969B;&#x10384;&#x10394;&#x10398;'.

	self assert: (stringWithHexNCRs asUtf8: true) = utf8! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 2/7/2013 22:32'!
unicodeCodePoint: codePoint
	"
	Answer nil if the Unicode codePoint is not a valid ISO 8859-15 character
	
	self assert: (Character unicodeCodePoint: 16r41) = $A.
	self assert: (Character unicodeCodePoint: 16r20AC) = $¤.
	"
	^ (self iso8859s15CodeForUnicodeCodePoint: codePoint)
		ifNotNil: [ :code | Character value: code ]! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 2/7/2013 21:20'!
storeObject: anObject
	"Set new contents on the clipboard.  Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times."
	contents _ (anObject isString or: [ anObject is: #Text])
		ifTrue: [ anObject withCuisLineEndings ]
		ifFalse: [ anObject copyForClipboard ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present"
	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface | | interface |
		interface _ clipboardInterface current.
		interface canStore ifTrue: [
			id _ self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat _ (self stringOrIdFor: contents) asUtf8.
	self primitiveClipboardString: primitiveFormat! !

!Clipboard methodsFor: 'private' stamp: 'jmv 2/7/2013 21:26'!
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
	^(String fromUtf8: primitiveFormat) withCuisLineEndings! !


!StringTest reorganize!
('testing' testCaseConversion testEncompassParagraph1 testEncompassParagraph2 testEncompassParagraph3 testEncompassParagraph4 testEncompassParagraph5 testLineSeparators testSorting)
('tests - converting' testBase64)
('UTF-8 conversion' testAsUtf8 testAsUtf8WithNCRs)
!

!methodRemoval: String #iso8859s15ToUtf8!
String removeSelector: #iso8859s15ToUtf8!
!methodRemoval: String #utf8ToISO8859s15!
String removeSelector: #utf8ToISO8859s15!
!methodRemoval: Integer class #evaluate:withUtf8BytesOfUnicodeCodePoint:!
Integer class removeSelector: #evaluate:withUtf8BytesOfUnicodeCodePoint:!
!methodRemoval: Integer class #nextUnicodeCodePointFromUtf8:!
Integer class removeSelector: #nextUnicodeCodePointFromUtf8:!
!methodRemoval: Integer class #unicodeCodePointOfUtfBytes:!
Integer class removeSelector: #unicodeCodePointOfUtfBytes:!
!methodRemoval: Integer class #utf8BytesOfUnicodeCodePoint:!
Integer class removeSelector: #utf8BytesOfUnicodeCodePoint:!

!CharacterTest reorganize!
('testing' testCaseConversion testSorting testTestingMethods)
('UTF-8 conversion' testFromUTF8 testSomeLatinCharsFromUTF8 testSomeLatinCharsToUTF8 testToUTF8)
!

