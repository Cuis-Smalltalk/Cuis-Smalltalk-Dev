'From Cuis 6.0 [latest update: #5201] on 26 May 2022 at 12:38:22 pm'!

!Character class methodsFor: 'converting' stamp: 'jmv 5/24/2022 16:10:06'!
utf8BytesAndCodePointFor: byte1 byte2: byte2 byte3: byte3 byte4: byte4 into: bytesBlock into: codePointBlock
	"Find next UTF-8 encoded code point in argument bytes.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence, consider the first byte an ISO-8859-15 Character (i.e. Cuis Character class encoding), and answer 1.
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| codePoint |
	byte1 < 128 ifTrue: [	"single byte"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: nil value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: byte1 ].
		^ 1 ].
	
	"At least 2 bytes"
	(byte2 notNil and: [ (byte2 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-15."
		^ self utf8BytesAndCodePointForIso8859s15: byte1 into: bytesBlock into: codePointBlock ].
	(byte1 bitAnd: 16rE0) = 16rC0  ifTrue: [ "two bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePoint _ ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63).
			codePointBlock value: codePoint ].
		^2 ].
	
	"At least 3 bytes"
	(byte3 notNil and: [ (byte3 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-15."
		^ self utf8BytesAndCodePointForIso8859s15: byte1 into: bytesBlock into: codePointBlock ].
	(byte1 bitAnd: 16rF0) = 16rE0  ifTrue: [ "three bytes"
		codePoint _  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
		codePoint = 16rFEFF ifTrue: [ "BOM. Ignore it. Skip ti."
			^3 ].
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: codePoint ].
		^3 ].

	"4 bytes"
	(byte4 notNil and: [ (byte4 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-15."
		^ self utf8BytesAndCodePointForIso8859s15: byte1 into: bytesBlock into: codePointBlock ].
	(byte1 bitAnd: 16rF8) = 16rF0  ifTrue: [  "four bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: byte4 ].
		codePointBlock ifNotNil: [
			codePoint _ ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12)
					+ ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63).
			codePointBlock value: codePoint ].
		^ 4].

	"invalid UTF-8. Assume byte1 is ISO8859-15."
	^ self utf8BytesAndCodePointForIso8859s15: byte1 into: bytesBlock into: codePointBlock.! !

!Character class methodsFor: 'converting' stamp: 'jmv 5/24/2022 16:58:19'!
utf8BytesAndCodePointForIso8859s15: iso8859s15 into: bytesBlock into: codePointBlock
	| codePoint |
	codePoint _ (Character numericValue: iso8859s15) codePoint.
	bytesBlock ifNotNil: [
		Character
			evaluateOnce: bytesBlock
			withUtf8BytesOfUnicodeCodePoint: codePoint ].
	codePointBlock ifNotNil: [
		codePointBlock value: codePoint ].
	^1! !


!String methodsFor: 'converting' stamp: 'jmv 5/21/2022 21:26:19'!
asByteString
	"Answer an instance of the Byte oriented String class"
	^self! !

!String methodsFor: 'converting' stamp: 'jmv 5/23/2022 17:31:16'!
asUtf32String
	"Answer an instance of the UTF-32 encoded String class"

	^Utf32String fromUtf8Bytes: self asUtf8Bytes! !

!String methodsFor: 'converting' stamp: 'jmv 5/23/2022 17:31:28'!
asUtf8String
	"Answer an instance of the UTF-8 encoded String class"

	^Utf8String fromUtf8Bytes: self asUtf8Bytes! !

!String methodsFor: 'enumerating' stamp: 'jmv 5/26/2022 11:30:56'!
collectInArray: aBlock
	"Refer to the comment in Collection|collect:."
	| result value |
	result _ Array new: self size.
	1 to: self size do: [ :index |
		value _ aBlock value: (self at: index).
		result at: index put: value].
	^ result.! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/24/2022 11:23:08'!
isValidInBinarySelectors
	(Character codePoint: value) ifNotNil: [ :ch | ^ch isValidInBinarySelectors ].
	^false! !


!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/24/2022 11:12:26'!
at: codePointIndex ifAbsent: absentBlock
	"Answer the UnicodeCodePoint at codePointIndex"

	(codePointIndex between: 1 and: self size) ifFalse: [ ^absentBlock value ].
	^self at: codePointIndex! !

!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/24/2022 11:15:42'!
last
	"Answer the last element of the receiver.
	Raise an error if the collection is empty."

	| size |
	(size _ self size) = 0 ifTrue: [self error: 'Empty Utf32String'].
	^ self at: size! !

!Utf32String methodsFor: 'copying' stamp: 'jmv 5/26/2022 11:34:36'!
appendToString: aString

	^Utf32String withWords: aString asUtf32Words, codePoints! !

!Utf32String methodsFor: 'copying' stamp: 'jmv 5/21/2022 22:02:37'!
appendToText: aText

	| textStringSize |
	textStringSize := aText string size.
	^ aText replaceFrom: textStringSize + 1
			 to: textStringSize 
			 with: self asText! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/23/2022 17:31:33'!
asUtf32String
	"Answer an instance of the UTF-32 encoded String class"

	^Utf32String fromUtf8Bytes: self asUtf8Bytes! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/23/2022 17:31:37'!
asUtf8String
	"Answer an instance of the UTF-8 encoded String class"

	^Utf8String fromUtf8Bytes: self asUtf8Bytes! !

!Utf32String methodsFor: 'enumerating' stamp: 'jmv 5/26/2022 11:31:19'!
collect: aBlock
	"Refer to the comment in Collection|collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacterLike ]) ifFalse: [
		^array ].
	^Utf32String newFrom: array! !

!Utf32String methodsFor: 'enumerating' stamp: 'jmv 5/26/2022 11:30:59'!
collectInArray: aBlock
	"Refer to the comment in Collection|collect:."
	| result i |
	result _ Array new: self size.
	i _ 1.
	self do: [ :codePoint |
		result at: i put: codePoint.
		i _ i + 1 ].
	^ result.! !

!Utf32String methodsFor: 'enumerating' stamp: 'jmv 5/21/2022 21:46:51'!
do: aBlock
	self from: 1 to: codePoints size do: aBlock! !


!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/21/2022 21:49:44'!
newFrom: aSimilarObject
	^Utf32String streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/24/2022 11:11:27'!
at: codePointIndex ifAbsent: absentBlock
	"Answer the UnicodeCodePoint at code point index codePointIndex (not a byte index!!)"

	(codePointIndex between: 1 and: self size) ifFalse: [ ^absentBlock value ].
	^self at: codePointIndex! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/24/2022 11:16:33'!
last
	"Answer the last element of the receiver.
	Raise an error if the collection is empty."

	| size |
	(size _ self size) = 0 ifTrue: [self error: 'Empty Utf8String'].
	^ self at: size! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 5/24/2022 18:05:15'!
appendToString: aString

	aString isStringLike ifFalse: [
'pufff' print.
	self halt ].
	^Utf8String withBytes: aString asUtf8Bytes, bytes! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 5/21/2022 22:02:34'!
appendToText: aText

	| textStringSize |
	textStringSize := aText string size.
	^ aText replaceFrom: textStringSize + 1
			 to: textStringSize 
			 with: self asText! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/23/2022 17:31:42'!
asUtf32String
	"Answer an instance of the UTF-32 encoded String class"

	^Utf32String fromUtf8Bytes: self asUtf8Bytes! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/23/2022 17:31:45'!
asUtf8String
	"Answer an instance of the UTF-8 encoded String class"

	^Utf8String fromUtf8Bytes: self asUtf8Bytes! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/21/2022 21:27:20'!
contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize _ smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	(Utf8String withBytes: 'A clear but rather long-winded summary' asUtf8) contractTo: 18
"! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/26/2022 11:33:05'!
displayStringOrText
	"To be used in the UI"
	"Answer the receiver itself."

	^self! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/22/2022 15:23:32'!
string
	"Answer the receiver itself.  This is for compatibility with other text classes."
	^self! !

!Utf8String methodsFor: 'enumerating' stamp: 'jmv 5/26/2022 11:31:22'!
collect: aBlock
	"Refer to the comment in Collection|collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacterLike ]) ifFalse: [
		^array ].
	^Utf8String newFrom: array! !

!Utf8String methodsFor: 'enumerating' stamp: 'jmv 5/26/2022 11:31:04'!
collectInArray: aBlock
	"Refer to the comment in Collection|collect:."
	| result i |
	result _ Array new: self size.
	i _ 1.
	self do: [ :codePoint |
		result at: i put: codePoint.
		i _ i + 1 ].
	^ result.! !

!Utf8String methodsFor: 'enumerating' stamp: 'jmv 5/21/2022 22:11:12'!
do: aBlock
	"Evaluate aBlock for all elements."

	| byteIndex stopByteIndex n |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	[ byteIndex <= stopByteIndex ] whileTrue: [
		n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			aBlock value: (UnicodeCodePoint codePoint: codePoint) ].
		byteIndex _ byteIndex + n ].! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/21/2022 21:48:34'!
newFrom: aSimilarObject
	^Utf8String streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]! !

!Utf8String class methodsFor: 'services' stamp: 'jmv 5/24/2022 12:22:31'!
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

!Utf8String class methodsFor: 'services' stamp: 'jmv 5/24/2022 12:22:38'!
utf8FromUtf32: aWordArray
	"Convert the given string from UTF-8 to UTF-32"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !


!Character class methodsFor: 'converting' stamp: 'jmv 5/24/2022 19:04:02'!
utf8BytesAndCodePointAt: byteIndex in: aByteArray into: bytesBlock into: codePointBlock
	"Find next UTF-8 encoded code point in utf8ByteArray starting at byte byteIndex.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence, consider the first byte an ISO-8859-15 Character (i.e. Cuis Character class encoding), and answer 1.
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| i s byte1 byte2 byte3 byte4 |
	
	s _ aByteArray size.
	i _ byteIndex.
	byte1 _ aByteArray at: i.
	i _ byteIndex+1.
	byte2 _ i <= s ifTrue: [aByteArray at: i].
	i _ byteIndex+2.
	byte3 _ i <= s ifTrue: [aByteArray at: i].
	i _ byteIndex+3.
	byte4 _ i <= s ifTrue: [aByteArray at: i].

	^self
		utf8BytesAndCodePointFor: byte1
		byte2: byte2
		byte3: byte3
		byte4: byte4
		into: bytesBlock
		into: codePointBlock! !


!String methodsFor: 'converting' stamp: 'jmv 5/24/2022 14:29:34'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray with a valid UTF-8 sequence.
	
	See #fromUtf8Bytes: "

	^self asUtf8Bytes: true.! !

!String methodsFor: 'enumerating' stamp: 'jmv 5/26/2022 11:31:11'!
collect: aBlock
	"Refer to the comment in Collection|collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c is: #Character ]) ifFalse: [
		^array ].
	^String newFrom: array! !


!Utf32String methodsFor: 'copying' stamp: 'jmv 5/26/2022 11:34:32'!
, aString
	^Utf32String withWords: codePoints, aString asUtf32Words.! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/24/2022 14:29:54'!
asUtf8Bytes
	"Answer a ByteArray with a valid UTF-8 sequence."

	^Utf8String utf8FromUtf32: codePoints! !


!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 11:00:12'!
fromUtf8Bytes: aByteArray

	^self withWords: (Utf8String utf32FromUtf8: aByteArray).! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/17/2022 15:05:27'!
at: codePointIndex
	"Answer the UnicodeCodePoint at code point index codePointIndex (not a byte index!!)"

	| byteIndex |
	self boundsCheck: codePointIndex.
	self isAscii ifTrue: [ ^ UnicodeCodePoint codePoint: (bytes at: codePointIndex) ].
	byteIndex _ self byteIndexAt: codePointIndex.
	Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
		^UnicodeCodePoint codePoint: codePoint ].
	^nil "Invalid UTF-8"! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/26/2022 11:00:07'!
asUtf32Words
	"Answer a WordArray."

	^Utf8String utf32FromUtf8: bytes! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/24/2022 14:30:01'!
asUtf8Bytes
	"Answer a ByteArray with a valid UTF-8 sequence."

	^bytes! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/26/2022 11:00:31'!
fromUtf32Words: aWordArray

	^self withBytes: (Utf8String utf8FromUtf32: aWordArray).! !

!methodRemoval: String class #utf8FromUtf32: stamp: 'jmv 5/26/2022 12:37:28'!
String class removeSelector: #utf8FromUtf32:!
!methodRemoval: String class #utf32FromUtf8: stamp: 'jmv 5/26/2022 12:37:28'!
String class removeSelector: #utf32FromUtf8:!
!methodRemoval: Character class #nextCodePointBytesFromUtf8:into: stamp: 'jmv 5/26/2022 12:37:28'!
Character class removeSelector: #nextCodePointBytesFromUtf8:into:!
!methodRemoval: Character class #nextUnicodeCodePointFromUtf8: stamp: 'jmv 5/26/2022 12:37:28'!
Character class removeSelector: #nextUnicodeCodePointFromUtf8:!
!methodRemoval: Character class #unicodeCodePointOfUtf8Bytes: stamp: 'jmv 5/26/2022 12:37:28'!
Character class removeSelector: #unicodeCodePointOfUtf8Bytes:!
