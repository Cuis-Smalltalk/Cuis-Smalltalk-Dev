'From Cuis 6.0 [latest update: #5256] on 10 June 2022 at 9:49:11 am'!

!Utf8String methodsFor: 'private' stamp: 'jmv 6/9/2022 12:01:04'!
privateBytes: utf8ByteArray codePointCount: aNumber
	"Initialize the receiver. If we already know that utf8ByteArray only holds bytes less than 128 (i.e. ASCII characters), initialize 'count'. This is an important optimization."

	bytes _ utf8ByteArray.
	count _ aNumber.
	strideIndex _ nil.
	hash _ nil.! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 13:19:50'!
fromUtf8Bytes: utf8ByteArray codePointCount: aNumber
	"Call this method if the number of CodePoints encoded in utf8ByteArray is known.
	This is an important optimization, especially if ASCII (i.e. utfByteArray size = aNumber).
	Note: The received ByteArray argument should not be modified elsewhere. Pass a copy if needed."

	^self basicNew privateBytes: utf8ByteArray codePointCount: aNumber.! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/9/2022 13:11:27'!
asUtf8String
	"Answer an instance of the UTF-8 encoded String class"

	| utf8Bytes |
	utf8Bytes _ self asUtf8Bytes.
	^Utf8String fromUtf8Bytes: utf8Bytes codePointCount: self size.! !


!String methodsFor: 'converting' stamp: 'jmv 6/9/2022 12:12:05'!
asUtf8Bytes
	"Convert the given string to UTF-8 from the internal encoding: ISO Latin 9 (ISO 8859-15)
	Answer a ByteArray with a valid UTF-8 sequence.
	Ignore any NCRs (Numeric Character References). See #fromUtf8Bytes: "

	| inStream nextChar codePoint |
	"Fast lane."
	self isAscii ifTrue: [ ^self asByteArray ].

	"Need to do UTF-8 encoding."
	^ByteArray streamContents: [ :outStream |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			codePoint _ nextChar codePoint.
			Character
				evaluate: [ :byte | outStream nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !


!String class methodsFor: 'instance creation' stamp: 'jmv 6/10/2022 09:28:43'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf8Bytes "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding."
	^self fromUtf8Bytes: aByteArray hex: false trimLastNull: false! !


!Utf32String methodsFor: 'copying' stamp: 'jmv 6/9/2022 14:43:15'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| epilogFirstIndex prologLastIndex |
	prologLastIndex _ startIndex-1.
	epilogFirstIndex _ endIndex+1.
	^Utf32String withWords:
		(codePoints copyFrom: 1 to: prologLastIndex),
		replacement asUtf32Words,
		(codePoints
			copyFrom: epilogFirstIndex
			to: codePoints size)! !


!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 6/10/2022 09:39:14'!
fromUtf8Bytes: aByteArray

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^self withWords: aByteArray asWordArray ].

	"Need to do UTF-8 decoding."
	^self withWords: (Utf8String utf32FromUtf8: aByteArray).! !


!Utf8String methodsFor: 'copying' stamp: 'jmv 6/9/2022 12:36:41'!
, aString
	^Utf8String fromUtf8Bytes: bytes, aString asUtf8Bytes codePointCount: self size + aString size.! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 6/9/2022 12:38:34'!
appendToString: aString

	^Utf8String fromUtf8Bytes: aString asUtf8Bytes, bytes codePointCount: aString size + self size.! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 6/9/2022 12:41:57'!
copyFrom: startIndex to: stopIndex
	"Return a new string, a copy of me from startIndex to stopIndex, both character indices.
	Signal SubscriptOutOfBounds or CollectionIsEmpty in case of illegal indices."
	
	| startByteIndex stopByteIndex |
	startIndex = (self size+1) ifTrue: [ ^Utf8String empty ].
	stopIndex = 0 ifTrue: [ ^Utf8String empty ].
	self boundsCheck: startIndex. 
	self boundsCheck: stopIndex. 
	startByteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	stopByteIndex := stopByteIndex + (self encodedByteCountAt: stopByteIndex) - 1.
	^Utf8String
		fromUtf8Bytes: (bytes copyFrom: startByteIndex to: stopByteIndex)
		codePointCount: stopIndex-startIndex+1.! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 6/9/2022 14:49:21'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| epilogFirstIndex prologLastIndex newBytes replacementBytes |
	prologLastIndex _ (self byteIndexAt: startIndex)-1.
	epilogFirstIndex _ self byteIndexAt: endIndex+1.
	replacementBytes _ replacement asUtf8Bytes.
	newBytes _ (bytes copyFrom: 1 to: prologLastIndex),
		replacementBytes,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size).
	^Utf8String fromUtf8Bytes: newBytes codePointCount: (startIndex-1) + replacement size + (self size-endIndex).! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 6/9/2022 13:00:37'!
copyWith: anUnicodeCodePoint

	^Utf8String
		fromUtf8Bytes: bytes, (Character utf8BytesOfUnicodeCodePoint: anUnicodeCodePoint codePoint)
		codePointCount: self size + 1.! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 6/9/2022 12:58:39'!
substrings
	"
	'  Hello World!! Some  		1234 more  ' substrings
	'  Hello World!! Some  		1234 more  ' asUtf8String substrings
	"
	^ self isAscii
		ifTrue: [ ('' substrings: bytes) collect: [ :eachBytes |
				Utf8String fromUtf8Bytes: eachBytes codePointCount: eachBytes size ]]
		ifFalse: [ ('' substrings: bytes) collect: [ :eachBytes |
				Utf8String fromUtf8Bytes: eachBytes ]].! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 6/10/2022 09:36:46'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	| correctedBytes |
	correctedBytes _ '' string: bytes withLineEndings: lineEndingString.
	"The codePointCount trick can be done because the line endings considered are all single byte UTF-8.
	If we ever consider unicode line endings taking more than one byte, this is no longer valid (pass nil instead)."
	^  Utf8String fromUtf8Bytes: correctedBytes codePointCount: self size - bytes size + correctedBytes size.! !

!Utf8String methodsFor: 'testing' stamp: 'jmv 6/10/2022 09:40:57'!
isAscii
	"Return true when I am pure 7-bit ASCII, which means each character is encoded as one byte."

	^ bytes size = self size! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/10/2022 09:44:05'!
byteIndexAt: codePointIndex
	| byteIndex |
	self isAscii ifTrue: [ ^ codePointIndex ].
	codePointIndex = (self size+1) ifTrue: [
		"The position a hypotetical extra code point would start at.
		Useful to find the last byte position for some codePointIndex. See senders."
		^bytes size+1 ].
	byteIndex _ codePointIndex > self stride
		ifTrue: [
			strideIndex ifNil: [ self computeCountAndIndex ].
			strideIndex at: codePointIndex - 1 // self stride ]
		ifFalse: [ 1 ].
	codePointIndex - 1 \\ self stride timesRepeat: [ 
		byteIndex _ byteIndex + (self encodedByteCountAt: byteIndex) ].
	^ byteIndex! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/10/2022 09:42:45'!
computeCountAndIndex
	self isEmpty ifTrue: [
		count _ 0.
		strideIndex _ #().
		^ self ].
	"the number of characters cannot be larger than the byte size"
	bytes size <= self stride
		ifTrue: [ self computeCountNoIndex ]
		ifFalse: [ self computeCountAndIndexFull ]! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/10/2022 09:47:15'!
computeCountAndIndexFull
	| byteIndex |
	count _ 0.
	strideIndex _ Array streamContents: [ :out |
		byteIndex _ 1.
		[ byteIndex <= bytes size ] whileTrue: [
			count _ count + 1.
			(count > self stride and: [ (count - 1 \\ self stride) = 0 ]) 
				ifTrue: [ out nextPut: byteIndex ].
			byteIndex _ byteIndex + (self encodedByteCountAt: byteIndex) ] ]! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/10/2022 09:41:56'!
computeCountNoIndex
	| byteIndex |
	count _ 0.
	strideIndex _ #().
	byteIndex _ 1.
	[ byteIndex <= bytes size ] whileTrue: [
		count _ count + 1.
		byteIndex _ byteIndex + (self encodedByteCountAt: byteIndex) ]! !

!Utf8String methodsFor: 'initialization' stamp: 'jmv 6/9/2022 12:30:42'!
initialize
	"Just an empty string."

	self privateBytes: `#[]` codePointCount: 0.! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 12:44:51'!
empty
	"Return the empty Utf8String instance.
	Since my instances are readonly, this could be cached."

	^ self fromUtf8Bytes: `#[]` codePointCount: 0.! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 12:54:51'!
fromUtf32Words: aWordArray

	^self fromUtf8Bytes: (Utf8String utf8FromUtf32: aWordArray) codePointCount: aWordArray size.! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 13:19:18'!
fromUtf8Bytes: utf8ByteArray
	"Prefer #fromUtf8Bytes:codePointCount: if the number of CodePoints is cheap to know.
	Performance optimization.
	Note: The received ByteArray argument should not be modified elsewhere. Pass a copy if needed."

	^self basicNew privateBytes: utf8ByteArray codePointCount: nil.! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 12:52:15'!
with: aCharacter 
	"Answer a new instance of me, containing utf bytes for aCharacter.
	Utf8String with: $a
	Utf8String with: $á
	Utf8String with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint)
		codePointCount: 1.! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 6/9/2022 12:51:46'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter and otherCharacter.
	Utf8String with: $a
	Utf8String with: $á with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint),
			(Character utf8BytesOfUnicodeCodePoint: otherCharacter codePoint)
		codePointCount: 2.! !


!Utf8Symbol methodsFor: 'converting' stamp: 'jmv 6/9/2022 13:12:48'!
asPlainString
	"Answer a plain String, not a Symbol."

	^Utf8String fromUtf8Bytes: bytes codePointCount: self size.! !


!Utf8Symbol class methodsFor: 'private' stamp: 'jmv 6/9/2022 12:33:24'!
basicFromString: aString
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	| utf8Bytes |
	utf8Bytes _ aString asUtf8Bytes.
	^self basicNew privateBytes: utf8Bytes codePointCount: aString size.! !


!Utf8EncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 6/9/2022 13:16:52'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Leave stream at the end position."

	| utf8 codePointCount |
	codePointCount _ 0.
	utf8 _ ByteArray streamContents: [ :utf8Stream |
		[ stream atEnd ] whileFalse: [
			stream nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
				codePointCount _ codePointCount + 1.
				utf8Stream nextPut: byte1.
				byte2 ifNotNil: [
					utf8Stream nextPut: byte2.
					byte3 ifNotNil: [
						utf8Stream nextPut: byte3.
						byte4 ifNotNil: [
							utf8Stream nextPut: byte4]]]]
				into: nil ]].
	^Utf8String fromUtf8Bytes: utf8 codePointCount: codePointCount.! !


!EncodedWriteStream methodsFor: 'accessing' stamp: 'jmv 6/10/2022 09:37:30'!
utf8Contents
	"Answer the Utf8String built so far."

	^Utf8String fromUtf8Bytes: self byteContents.! !


!EncodedByteArrayStreamTest methodsFor: 'tests' stamp: 'jmv 6/9/2022 13:18:20'!
test01

	| utf8String utf32String stringAsIso8859s15 bytes1 bytes2 bytes3 |
	utf8String _ Utf8String fromUtf8Bytes: 'Hello World, ñandú, agüita. ' asUtf8Bytes, #[208 155 208 190 209 128 208 181 208 188].
	utf32String _ Utf32String withWords: utf8String asUtf32Words.
	stringAsIso8859s15 _ utf8String bytes asString.							"Includes random content, String can't handle full Unicode"

	bytes1 _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: utf8String ].
	self assert: ((EncodedReadStream on: bytes1) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes1) useBytes; contents) equals: bytes1.
	self assert: ((EncodedReadStream on: bytes1) useUtf32String; contents) equals: utf32String.
	self assert: ((EncodedReadStream on: bytes1) useCharacters; contents) equals: stringAsIso8859s15.

	bytes2 _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: utf8String bytes ].
	self assert: ((EncodedReadStream on: bytes2) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes2) useBytes; contents) equals: bytes2.
	self assert: ((EncodedReadStream on: bytes2) useUtf32String; contents) equals: utf32String.
	self assert: ((EncodedReadStream on: bytes2) useCharacters; contents) equals: stringAsIso8859s15.

	bytes3 _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: utf32String ].
	self assert: ((EncodedReadStream on: bytes3) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes3) useBytes; contents) equals: bytes3.
	self assert: ((EncodedReadStream on: bytes3) useUtf32String; contents) equals: utf32String.
	self assert: ((EncodedReadStream on: bytes3) useCharacters; contents) equals: stringAsIso8859s15.! !

!EncodedByteArrayStreamTest methodsFor: 'tests' stamp: 'jmv 6/9/2022 13:18:24'!
test02
	"Invalid UTF-8, but possible saved as reasonable ISO 8859-15, with use of full 8 bit range.
	To test invalid utf8 detection, and conversion from 8859-15 to utf."

	| invalidUtf8Bytes validUtf8Bytes utf8String utf32String bytes |

	"Second byte is invalid UTF-8. Created from a String (i.e. ISO 8859-15 bytes), and a valid UTF-8 ByteArray:
	'uña ' asByteArray, #[206 188]"
	invalidUtf8Bytes _ #[117 241 97 32 206 188].

	"A correct UTF-8 sequence, created by correct conversion of the String above, and the same valid UTF-8 coda.
	'uña ' asUtf8Bytes, #[206 188]"
	validUtf8Bytes _ #[117 195 177 97 32 206 188].
	
	utf8String _ Utf8String fromUtf8Bytes: validUtf8Bytes.
	utf32String _ Utf32String withWords: utf8String asUtf32Words.

	bytes _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: invalidUtf8Bytes ].
	self assert: ((EncodedReadStream on: bytes) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes) useBytes; contents) equals: bytes.
	self assert: ((EncodedReadStream on: bytes) useUtf32String; contents) equals: utf32String.! !


!EncodedFileStreamTest methodsFor: 'tests' stamp: 'jmv 6/9/2022 13:18:28'!
test01

	| bytes utf8String utf32String stringAsIso8859s15 fe1 fe2 fe3 |
	bytes _ 'Hello World, ñandú, agüita. ' asUtf8Bytes, #[208 155 208 190 209 128 208 181 208 188].
	utf8String _ Utf8String fromUtf8Bytes: bytes.
	utf32String _ Utf32String withWords: utf8String asUtf32Words.
	stringAsIso8859s15 _ utf8String bytes asString.							"Includes random content, String can't handle full Unicode"

	fe1 _ 'xxtes01tUtf8String.txt' asFileEntry.
	fe1 forceWriteStreamDo: [ :stream | stream nextPutAll: utf8String ].
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe1 readStreamDo: [ :strm | strm useBytes; contents ]) equals: bytes.
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.
	self assert: (fe1 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: stringAsIso8859s15.

	fe2 _ 'xxtest01Utf8Bytes.txt' asFileEntry.
	fe2 forceWriteStreamDo: [ :stream | stream nextPutAll: utf8String bytes ].
	self assert: (fe2 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe2 readStreamDo: [ :strm | strm useBytes; contents ]) equals: bytes.
	self assert: (fe2 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.
	self assert: (fe2 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: stringAsIso8859s15.

	fe3 _ 'xxtes01tUtf32String.txt' asFileEntry.
	fe3 forceWriteStreamDo: [ :stream | stream nextPutAll: utf32String ].
	self assert: (fe3 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe3 readStreamDo: [ :strm | strm useBytes; contents ]) equals: bytes.
	self assert: (fe3 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.
	self assert: (fe3 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: stringAsIso8859s15.

	fe1 delete.
	fe2 delete.
	fe3 delete.! !

!EncodedFileStreamTest methodsFor: 'tests' stamp: 'jmv 6/9/2022 13:18:38'!
test02
	"Invalid UTF-8, but possible saved as reasonable ISO 8859-15, with use of full 8 bit range.
	To test invalid utf8 detection, and conversion from 8859-15 to utf."

	| invalidUtf8Bytes validUtf8Bytes utf8String utf32String fe1 |

	"Second byte is invalid UTF-8. Created from a String (i.e. ISO 8859-15 bytes), and a valid UTF-8 ByteArray:
	'uña ' asByteArray, #[206 188]"
	invalidUtf8Bytes _ #[117 241 97 32 206 188].

	"A correct UTF-8 sequence, created by correct conversion of the String above, and the same valid UTF-8 coda.
	'uña ' asUtf8Bytes, #[206 188]"
	validUtf8Bytes _ #[117 195 177 97 32 206 188].
	
	utf8String _ Utf8String fromUtf8Bytes: validUtf8Bytes.
	utf32String _ Utf32String withWords: utf8String asUtf32Words.
	
	fe1 _ 'xxtest02InvalidUtf8Bytes.txt' asFileEntry.
	fe1 forceWriteStreamDo: [ :stream | stream nextPutAll: invalidUtf8Bytes ].
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe1 readStreamDo: [ :strm | strm useBytes; contents ]) equals: invalidUtf8Bytes.
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.

	fe1 delete.! !

!methodRemoval: Utf8String class #withBytes: stamp: 'jmv 6/10/2022 09:38:09'!
Utf8String class removeSelector: #withBytes:!
!methodRemoval: Utf8String class #new: stamp: 'jmv 6/10/2022 09:37:02'!
Utf8String class removeSelector: #new:!
!methodRemoval: Utf8String #beAscii stamp: 'jmv 6/10/2022 09:32:06'!
Utf8String removeSelector: #beAscii!
!methodRemoval: Utf8String #bytes: stamp: 'jmv 6/10/2022 09:38:15'!
Utf8String removeSelector: #bytes:!
!methodRemoval: Utf32String class #new: stamp: 'jmv 6/10/2022 09:37:02'!
Utf32String class removeSelector: #new:!
