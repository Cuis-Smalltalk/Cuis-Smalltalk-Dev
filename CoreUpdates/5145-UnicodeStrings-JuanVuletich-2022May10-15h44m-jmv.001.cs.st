'From Cuis 6.0 [latest update: #5143] on 10 May 2022 at 3:48:38 pm'!
!classDefinition: #StringUtf32 category: 'System-Text'!
Object subclass: #StringUtf32
	instanceVariableNames: 'codePoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!StringUtf32 commentStamp: 'jmv 5/10/2022 15:47:34' prior: 0!
String like objects handling Unicode code points, encoded as UTF-32 bytes.

Install VectorGraphics / TrueType support, and play with the class side demo methods.!

!classDefinition: #StringUtf8 category: 'System-Text'!
Object subclass: #StringUtf8
	instanceVariableNames: 'bytes count index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!StringUtf8 commentStamp: 'jmv 5/10/2022 15:47:45' prior: 0!
String like objects handling Unicode code points, encoded as UTF-8 bytes.

Install VectorGraphics / TrueType support, and play with the class side demo methods.!

!classDefinition: #UnicodeCodePoint category: 'System-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!UnicodeCodePoint commentStamp: 'jmv 5/10/2022 15:48:21' prior: 0!
An Unicode Code Point can behave in a somewhat similar way to a Character.!


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 5/5/2022 12:08:53'!
scanStringUtf32From: startIndex to: stopIndex in: aStringUtf32 rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX |
	lastIndex _ startIndex.
	aStringUtf32 words from: startIndex to: stopIndex do: [ :code |
		"stops are only defined for the first 128 characters
		(entries 128 to 255 are always nil)."
		code < 256 ifTrue: [
			(stops at: code + 1) ifNotNil: [ :stop | ^stop ]].
		nextDestX _ destX + (font widthOfCodePoint: code).
		nextDestX > rightX ifTrue: [
			^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !

!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 5/5/2022 12:08:06'!
scanStringUtf8From: startIndex to: stopIndex in: aStringUtf8 rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX |
	lastIndex _ startIndex.
	aStringUtf8 from: startIndex to: stopIndex doWithUtf8Bytes: [ :byte1 :byte2 :byte3 :byte4 |
		"stops are only defined for the first 128 characters
		(entries 128 to 255 are always nil).
		It is OK to just use the first byte in UTF-8"
		(stops at: byte1 + 1) ifNotNil: [ :stop | ^stop ].
		nextDestX _ destX + (font widthOfUtf8Byte1: byte1 b2: byte2 b3: byte3 b4: byte4).
		nextDestX > rightX ifTrue: [
			^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !


!StringUtf32 methodsFor: 'accessing' stamp: 'jmv 5/5/2022 10:23:16'!
at: codePointIndex
	"Answer the UnicodeCodePoint at codePointIndex"

	^UnicodeCodePoint codePoint: (codePoints at: codePointIndex)! !

!StringUtf32 methodsFor: 'accessing' stamp: 'jmv 5/4/2022 15:45:00'!
isLastLineSeparator
	"
	(StringUtf8 withBytes: #[64 65 66]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 10]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 13]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 12]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 194 133]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 168 ]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 169 ]) isLastLineSeparator
	"
	| s |
	s _ codePoints size.
	s = 0 ifTrue: [ ^false ].
	(#(10 13 12 16r0085 16r2028 16r2029) statePointsTo: 	(codePoints at: s)) ifTrue: [ ^true ].
	^false.
	! !

!StringUtf32 methodsFor: 'accessing' stamp: 'jmv 5/4/2022 15:47:44'!
isLastSeparator
"A few more are missing!!"
	"
	(StringUtf8 withBytes: #[64 65 66]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 32]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 9]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 10]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 13]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 12]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 194 133]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 168 ]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 169 ]) isLastSeparator
	"
	| s |
	s _ codePoints size.
	s = 0 ifTrue: [ ^false ].
	(#(32 9) statePointsTo: 	(codePoints at: s)) ifTrue: [ ^true ].
	self isLastLineSeparator ifTrue: [ ^true ].
	^false.
	! !

!StringUtf32 methodsFor: 'accessing' stamp: 'jmv 5/4/2022 15:48:00'!
size
	^codePoints size! !

!StringUtf32 methodsFor: 'accessing' stamp: 'jmv 5/4/2022 19:22:10'!
words
	^codePoints! !

!StringUtf32 methodsFor: 'accessing' stamp: 'jmv 5/5/2022 10:28:47'!
words: aWordArray
	codePoints _ aWordArray! !

!StringUtf32 methodsFor: 'copying' stamp: 'jmv 5/4/2022 19:22:55'!
, aStringUtf32
	^StringUtf32 withWords: codePoints, aStringUtf32 words.! !

!StringUtf32 methodsFor: 'copying' stamp: 'jmv 5/4/2022 15:34:58'!
copyFrom: startIndex to: endIndex 
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."
	
	^StringUtf32 withWords:
		(codePoints
			copyFrom: startIndex
			to: endIndex)! !

!StringUtf32 methodsFor: 'copying' stamp: 'jmv 5/4/2022 17:29:35'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| epilogFirstIndex prologLastIndex |
	prologLastIndex _ startIndex-1.
	epilogFirstIndex _ endIndex+1.
	^StringUtf32 withWords:
		(codePoints copyFrom: 1 to: prologLastIndex),
		replacement asUtf32,
		(codePoints
			copyFrom: epilogFirstIndex
			to: codePoints size)! !

!StringUtf32 methodsFor: 'comparing' stamp: 'jmv 5/5/2022 10:34:05'!
= aStringUtf32
	"Answer whether the receiver sorts equally as aStringUtf32.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aStringUtf32 ifTrue: [ ^ true ].

	"If argument is not a aStringUtf32, maybe it is a Text?"
	aStringUtf32 class == StringUtf32 ifFalse: [
		(aStringUtf32 is: #Text) ifTrue: [ ^ self = aStringUtf32 string ].
		^ false].

	self size = aStringUtf32 size ifFalse: [
		^false ].
	
	self size > 256 ifTrue: [
		codePoints hashQuick = aStringUtf32 words hashQuick ifFalse: [ ^false ]].

	^ codePoints = aStringUtf32 words! !

!StringUtf32 methodsFor: 'comparing' stamp: 'jmv 5/5/2022 10:36:15'!
hash
	"Hash is reimplemented because = is implemented."

	^codePoints hash! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:16'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8: self asUtf8! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/4/2022 19:06:03'!
asPlainString
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:46'!
asString
	"Answer the receiver itself."
	^self! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/4/2022 15:19:36'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:28'!
asUtf32
	"Answer a WordArray."

	^codePoints! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:07:15'!
asUtf32: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-32 doesn't need them after all.
	Answer a WordArray."

	^codePoints! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:03:25'!
asUtf8
	"Answer a ByteArray."

	^String utf8FromUtf32: codePoints! !

!StringUtf32 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:03'!
asUtf8: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-32 doesn't need them after all.
	Answer a ByteArray."

	^self asUtf8! !

!StringUtf32 methodsFor: 'enumerating' stamp: 'jmv 5/5/2022 11:25:41'!
from: start to: stop do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	start to: stop do: [ :index | aBlock value: (self at: index) ].! !

!StringUtf32 methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:28:36'!
is: aSymbol
{ #is: . self class. aSymbol} print.
	^ super is: aSymbol! !

!StringUtf32 methodsFor: 'testing' stamp: 'jmv 5/4/2022 15:19:54'!
isEmpty
	^codePoints isEmpty! !

!StringUtf32 methodsFor: 'testing' stamp: 'jmv 5/4/2022 15:19:36'!
isStringLike

	^true! !

!StringUtf32 methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 16:56:56'!
encompassLine: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| left rightCr rightNull |
	left _ (codePoints lastIndexOf: `Character newLineCharacter numericValue` startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr _ (codePoints indexOf: `Character newLineCharacter numericValue` startingAt: (anInterval last max: anInterval first) ifAbsent: [codePoints size+1])-1.
	rightNull _ (codePoints indexOf: 0 startingAt: (anInterval last max: anInterval first) ifAbsent: [codePoints size+1])-1.
	^left to: (rightCr min: rightNull)! !

!StringUtf32 methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 15:32:57'!
encompassParagraph: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| left rightCr rightNull |
	left _ (codePoints lastIndexOf: `Character newLineCharacter numericValue` startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr _ (codePoints indexOf: `Character newLineCharacter numericValue` startingAt: (anInterval last max: anInterval first) ifAbsent: [codePoints size]).
	rightNull _ (codePoints indexOf: 0 startingAt: (anInterval last max: anInterval first) ifAbsent: [codePoints size+1])-1.
	^left to: (rightCr min: rightNull)! !

!StringUtf32 methodsFor: 'character scanner support' stamp: 'jmv 5/5/2022 11:13:45'!
scanCharactersFrom: startIndex to: stopIndex with: aCharacterScanner rightX: rightX stopConditions: stops
	"Double dispatch."

	^ aCharacterScanner scanStringUtf32From: startIndex to: stopIndex in: self rightX: rightX stopConditions: stops! !

!StringUtf32 methodsFor: 'as yet unclassified' stamp: 'jmv 5/10/2022 15:43:00'!
doesNotUnderstand: aMessage

	{ 'StringUtf32 does not understand: '. aMessage} print! !


!StringUtf32 class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:19:26'!
fromUtf32: words

	^self withWords: words! !

!StringUtf32 class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:15:48'!
fromUtf8: aByteArray

	^self withWords: (String utf32FromUtf8: aByteArray).! !

!StringUtf32 class methodsFor: 'instance creation' stamp: 'jmv 5/4/2022 17:19:49'!
with: aCharacter 
	"Answer a new instance of me, containing utf bytes for aCharacter.
	StringUtf32 with: $a
	StringUtf32 with: $á
	StringUtf32 with: Character euro
	"
	^self withWords: (WordArray with: aCharacter codePoint).! !

!StringUtf32 class methodsFor: 'instance creation' stamp: 'jmv 5/4/2022 17:19:31'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter.
	StringUtf32 with: $a
	StringUtf32 with: $á with: Character euro
	"
	^self withWords: (WordArray with: aCharacter codePoint with: otherCharacter codePoint).! !

!StringUtf32 class methodsFor: 'instance creation' stamp: 'jmv 5/4/2022 15:49:33'!
withWords: aWordArray
	^self new words: aWordArray ! !


!StringUtf8 methodsFor: 'accessing' stamp: 'jmv 5/6/2022 18:48:36'!
at: codePointIndex
	"Answer the UnicodeCodePoint at code point index codePointIndex (not a byte index!!)"

	| byteIndex |
	self boundsCheck: codePointIndex.
	self isAscii ifTrue: [ ^ UnicodeCodePoint codePoint: (bytes at: codePointIndex) ].
	byteIndex _ self byteIndexAt: codePointIndex.
	Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
		^UnicodeCodePoint codePoint: codePoint ].! !

!StringUtf8 methodsFor: 'accessing' stamp: 'jmv 5/4/2022 19:22:50'!
bytes
	^bytes! !

!StringUtf8 methodsFor: 'accessing' stamp: 'jmv 5/5/2022 10:23:51'!
bytes: aByteArray
	bytes _ aByteArray! !

!StringUtf8 methodsFor: 'accessing' stamp: 'jmv 5/4/2022 15:46:34'!
isLastLineSeparator
	"
	(StringUtf8 withBytes: #[64 65 66]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 10]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 13]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 12]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 194 133]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 168 ]) isLastLineSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 169 ]) isLastLineSeparator
	"
	| s butButLastByte butLastByte lastByte |
	s _ bytes size.
	s = 0 ifTrue: [ ^false ].
	(#(10 13 12) statePointsTo: 	(bytes at: s)) ifTrue: [ ^true ].
	s = 1 ifTrue: [ ^false ].
	lastByte _ bytes at: s.
	butLastByte _ bytes at: s-1.
	(butLastByte = 16rC2 and: [ lastByte = 16r85 ]) ifTrue: [ ^true ]. "Next Line, NEL"
	s = 2 ifTrue: [ ^false ].
	butButLastByte _ bytes at: s-2.
	(butButLastByte = 16rE2 and: [butLastByte = 16r80 and: [ "Line Separator and Paragraph Separator"
		lastByte = 16rA8 or: [lastByte = 16rA9]]]) ifTrue: [ ^true ].
	^false.
	! !

!StringUtf8 methodsFor: 'accessing' stamp: 'jmv 5/4/2022 15:46:58'!
isLastSeparator
"A few more are missing!!"
	"
	(StringUtf8 withBytes: #[64 65 66]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 32]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 9]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 10]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 13]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 12]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 194 133]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 168 ]) isLastSeparator
	(StringUtf8 withBytes: #[64 65 66 226 128 169 ]) isLastSeparator
	"
	| s |
	s _ bytes size.
	s = 0 ifTrue: [ ^false ].
	(#(32 9) statePointsTo: 	(bytes at: s)) ifTrue: [ ^true ].
	self isLastLineSeparator ifTrue: [ ^true ].
	^false.
	! !

!StringUtf8 methodsFor: 'accessing' stamp: 'svc 5/5/2022 15:51:50'!
size
	"Return how many characters I contain"
	
	self isEmpty ifTrue: [ ^ 0 ].
	count ifNil: [ self computeCountAndIndex ].
	^ count! !

!StringUtf8 methodsFor: 'copying' stamp: 'jmv 5/4/2022 19:22:52'!
, aStringUtf8
	^StringUtf8 withBytes: bytes, aStringUtf8 bytes.! !

!StringUtf8 methodsFor: 'copying' stamp: 'svc 5/5/2022 16:25:23'!
copyFrom: startIndex to: stopIndex
	"Return a new string, a copy of me from startIndex to stopIndex, both character indices.
	Signal SubscriptOutOfBounds or CollectionIsEmpty in case of illegal indices."
	
	| startByteIndex stopByteIndex substring |
	startIndex = (self size+1) ifTrue: [ ^StringUtf8 empty ].
	stopIndex = 0 ifTrue: [ ^StringUtf8 empty ].
	self boundsCheck: startIndex. 
	self boundsCheck: stopIndex. 
	startByteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	stopByteIndex := stopByteIndex + (self encodedByteCountAt: stopByteIndex) - 1.
	substring := StringUtf8 withBytes: (bytes copyFrom: startByteIndex to: stopByteIndex).
	self isAscii ifTrue: [ substring beAscii ].
	^ substring! !

!StringUtf8 methodsFor: 'copying' stamp: 'jmv 5/5/2022 16:57:30'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| epilogFirstIndex prologLastIndex |
	prologLastIndex _ (self byteIndexAt: startIndex)-1.
	epilogFirstIndex _ self byteIndexAt: endIndex+1.
	^StringUtf8 withBytes:
		(bytes copyFrom: 1 to: prologLastIndex),
		replacement asUtf8,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size)! !

!StringUtf8 methodsFor: 'comparing' stamp: 'jmv 5/5/2022 10:35:30'!
= aStringUtf8
	"Answer whether the receiver sorts equally as aStringUtf8.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aStringUtf8 ifTrue: [ ^ true ].

	"If argument is not a aStringUtf32, maybe it is a Text?"
	aStringUtf8 class == StringUtf8 ifFalse: [
		(aStringUtf8 is: #Text) ifTrue: [ ^ self = aStringUtf8 string ].
		^ false].

	bytes size = aStringUtf8 bytes size ifFalse: [
		^false ].
	
	bytes size > 256 ifTrue: [
		bytes hashQuick = aStringUtf8 bytes hashQuick ifFalse: [ ^false ]].

	^ bytes = aStringUtf8 bytes! !

!StringUtf8 methodsFor: 'comparing' stamp: 'jmv 5/5/2022 10:38:57'!
hash
	"Hash is reimplemented because = is implemented."

	^bytes hash! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:03'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8: bytes! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 5/4/2022 19:06:11'!
asPlainString
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:44'!
asString
	"Answer the receiver itself."
	^self! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 3/13/2022 18:50:31'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:39'!
asUtf32
	"Answer a WordArray."

	^String utf32FromUtf8: bytes! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:07:29'!
asUtf32: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-32 doesn't need them after all.
	Answer a WordArray."

	^self asUtf32! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:03:32'!
asUtf8
	"Answer a ByteArray."

	^bytes! !

!StringUtf8 methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:10'!
asUtf8: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-8 doesn't need them after all.
	Answer a ByteArray."

	^bytes! !

!StringUtf8 methodsFor: 'enumerating' stamp: 'jmv 5/5/2022 16:57:33'!
from: startIndex to: stopIndex do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	| byteIndex n cpIndex |
	byteIndex _ self byteIndexAt: startIndex.
	cpIndex _ startIndex.
	[ cpIndex <= stopIndex ] whileTrue: [
		n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			aBlock value: (UnicodeCodePoint codePoint: codePoint).
			cpIndex _ cpIndex + 1 ].
		byteIndex _ byteIndex + n ].! !

!StringUtf8 methodsFor: 'enumerating' stamp: 'jmv 5/5/2022 16:57:37'!
from: startIndex to: stopIndex doWithUtf8Bytes: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	| byteIndex n stopByteIndex |
	byteIndex _ self byteIndexAt: startIndex.
	stopByteIndex _ self byteIndexAt: stopIndex.
	[ byteIndex <= stopByteIndex ] whileTrue: [
		n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: aBlock into: nil.
		byteIndex _ byteIndex + n ].! !

!StringUtf8 methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:28:32'!
is: aSymbol
{ #is: . self class. aSymbol} print.
	^ super is: aSymbol! !

!StringUtf8 methodsFor: 'testing' stamp: 'svc 5/5/2022 16:12:43'!
isAscii
	"Return true when I am pure 7-bit ASCII, which means each character is encoded as one byte."

	count ifNil: [ self computeCountAndIndex ].
	^ bytes size = count! !

!StringUtf8 methodsFor: 'testing' stamp: 'jmv 5/3/2022 12:12:52'!
isEmpty
	^bytes isEmpty! !

!StringUtf8 methodsFor: 'testing' stamp: 'jmv 4/29/2022 11:35:03'!
isStringLike

	^true! !

!StringUtf8 methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 16:57:00'!
encompassLine: anCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte rightNullByte |
	argumentLeftByte _ self byteIndexAt: anCodePointInterval first.
	argumentRightByte _ self byteIndexAt: anCodePointInterval last.
	leftByte _ (bytes lastIndexOf: `Character newLineCharacter numericValue` startingAt: argumentLeftByte - 1 ifAbsent:[0]) + 1.
	rightCrByte _ (bytes indexOf: `Character newLineCharacter numericValue` startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size+1])-1.
	rightNullByte _ (bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size+1])-1.
	^(self codePointIndexAt: leftByte) to: (self codePointIndexAt: (rightCrByte min: rightNullByte))! !

!StringUtf8 methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 16:57:40'!
encompassParagraph: anCodePointInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).
	Look also for null characters. Never include null characters in the answer.
	See also #encompassLine:"
	| argumentLeftByte argumentRightByte leftByte rightCrByte rightNullByte |
	argumentLeftByte _ self byteIndexAt: anCodePointInterval first.
	argumentRightByte _ self byteIndexAt: anCodePointInterval last.
	leftByte _ (bytes lastIndexOf: `Character newLineCharacter numericValue` startingAt: argumentLeftByte - 1 ifAbsent:[0]) + 1.
	rightCrByte _ (bytes indexOf: `Character newLineCharacter numericValue` startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size]).
	rightNullByte _ (bytes indexOf: 0 startingAt: (argumentRightByte max: argumentLeftByte) ifAbsent: [bytes size+1])-1.
	^(self codePointIndexAt: leftByte) to: (self codePointIndexAt: (rightCrByte min: rightNullByte))! !

!StringUtf8 methodsFor: 'character scanner support' stamp: 'jmv 5/5/2022 11:13:55'!
scanCharactersFrom: startIndex to: stopIndex with: aCharacterScanner rightX: rightX stopConditions: stops
	"Double dispatch."

	^ aCharacterScanner scanStringUtf8From: startIndex to: stopIndex in: self rightX: rightX stopConditions: stops! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 15:53:18'!
beAscii
	"Mark me as being pure 7-bit ASCII, which means each character is encoded as one byte"
	
	count := bytes size.
	index := #().! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 16:04:35'!
boundsCheck: codePointIndex
	(codePointIndex between: 1 and: self size) 
		ifFalse: [ self errorSubscriptBounds: codePointIndex ]! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 16:25:57'!
byteIndexAt: codePointIndex
	| byteIndex |
	self isAscii ifTrue: [ ^ codePointIndex ].
	codePointIndex = (self size+1) ifTrue: [
		"The position a hypotetical extra code point would start at.
		Useful to find the last byte position for some codePointIndex. See senders."
		^bytes size+1 ].
	byteIndex := codePointIndex > self stride
		ifTrue: [
			index at: codePointIndex - 1 // self stride ]
		ifFalse: [ 1 ].
	codePointIndex - 1 \\ self stride timesRepeat: [ 
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ].
	^ byteIndex! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 16:46:33'!
codePointIndexAt: byteIndex
	| codePointIndex byteCursor |
	self isAscii ifTrue: [ ^ byteIndex ].
	codePointIndex := byteCursor := 1.
	index ifNil: [ self computeCountAndIndex ].
	index ifNotEmpty: [  
		index 
			findBinaryIndex: [ :x | byteIndex - x ] 
			do: [ :found | ^ (found * self stride) + 1 ] 
			ifNone: [ :lower :upper |
				lower = 0 
			 		ifFalse: [ 
						byteCursor := index at: lower. 
						codePointIndex := (lower * self stride) + 1 ] ] ].
	[ byteCursor <= bytes size ] whileTrue: [
		byteCursor = byteIndex 
			ifTrue: [ ^ codePointIndex ].
		byteCursor := byteCursor + (self encodedByteCountAt: byteCursor).
		codePointIndex := codePointIndex + 1 ].
	^ 0! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 15:45:39'!
computeCountAndIndex
	self isEmpty ifTrue: [ count := 0. ^ self ].
	"the number of characters cannot be larger than the byte size"
	bytes size <= self stride
		ifTrue: [ self computeCountNoIndex ]
		ifFalse: [ self computeCountAndIndexFull ]! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 15:46:55'!
computeCountAndIndexFull
	count := 0.
	index := Array streamContents: [ :out | | byteIndex |
		byteIndex := 1.
		[ byteIndex <= bytes size ] whileTrue: [
			count := count + 1.
			(count > self stride and: [ (count - 1 \\ self stride) = 0 ]) 
				ifTrue: [ out nextPut: byteIndex ].
			byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ] ]! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 15:46:18'!
computeCountNoIndex
	| byteIndex |
	count := 0.
	index := #().
	byteIndex := 1.
	[ byteIndex <= bytes size ] whileTrue: [
		count := count + 1.
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ]! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 15:50:23'!
encodedByteCountAt: byteIndex
	| byte |
	(byte := bytes at: byteIndex) < 128 ifTrue: [ ^ 1 ].
	(byte bitAnd: 2r11100000) = 2r11000000 ifTrue: [ ^ 2 ].
	(byte bitAnd: 2r11110000) = 2r11100000 ifTrue: [ ^ 3 ].
	(byte bitAnd: 2r11111000) = 2r11110000 ifTrue: [ ^ 4 ].
	self error: 'IllegalLeadingByte '! !

!StringUtf8 methodsFor: 'private' stamp: 'svc 5/5/2022 15:45:54'!
stride
	"Return the stride, the constant I use internally to build my index"
	
	^ 32! !

!StringUtf8 methodsFor: 'as yet unclassified' stamp: 'jmv 5/10/2022 15:43:04'!
doesNotUnderstand: aMessage

	{ 'StringUtf8 does not understand: '. aMessage} print! !


!StringUtf8 class methodsFor: 'instance creation' stamp: 'svc 5/5/2022 16:25:07'!
empty
	"Return the empty StringUtf8 instance.
	Since my instances are readonly, this could be cached."

	^ (self withBytes: #[]) beAscii; yourself! !

!StringUtf8 class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:16:43'!
fromUtf32: aWordArray

	^self withBytes: (String utf8FromUtf32: aWordArray).! !

!StringUtf8 class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:19:21'!
fromUtf8: bytes

	^self withBytes: bytes! !

!StringUtf8 class methodsFor: 'instance creation' stamp: 'jmv 4/29/2022 15:04:19'!
with: aCharacter 
	"Answer a new instance of me, containing utf bytes for aCharacter.
	StringUtf8 with: $a
	StringUtf8 with: $á
	StringUtf8 with: Character euro
	"
	^self withBytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint).! !

!StringUtf8 class methodsFor: 'instance creation' stamp: 'jmv 4/29/2022 15:04:23'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter.
	StringUtf8 with: $a
	StringUtf8 with: $á with: Character euro
	"
	^self withBytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint),
		(Character utf8BytesOfUnicodeCodePoint: otherCharacter codePoint).! !

!StringUtf8 class methodsFor: 'instance creation' stamp: 'jmv 5/3/2022 12:10:29'!
withBytes: aByteArray
	^self new bytes: aByteArray ! !


!UnicodeCodePoint methodsFor: 'accessing' stamp: 'jmv 5/5/2022 15:20:14'!
codePoint
	^value! !

!UnicodeCodePoint methodsFor: 'accessing' stamp: 'jmv 5/4/2022 16:54:26'!
codePoint: aNumber
	value _ aNumber! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:28:25'!
is: aSymbol
{ #is: . self class. aSymbol} print.
	^ super is: aSymbol! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:24:25'!
isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."
	^self isLetter or: [self isDigit]! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:21:35'!
isDigit
	"Answer whether the receiver is a digit."

	^ value between: 48 and: 57! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:23:34'!
isLetter
	"Answer whether the receiver is a letter."
self flag: #unicodeNeedsImprovement.
	(Character codePoint: value) ifNotNil: [ :ch | ^ch isLetter ].
	^false! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 16:59:07'!
isLineSeparator
	"Answer whether the receiver is a line separator character:
	line feed, cr, or form feed."

	^ #(10 13 12) statePointsTo: value! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 16:59:12'!
isSeparator
	"Answer whether the receiver is one of the separator characters
	space, tab, lf, cr, or form feed."

	^ #(32 9 10 13 12) statePointsTo: value! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:25:12'!
isValidInIdentifiers
	"Can I be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ $_ numericValue = value ]! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:25:50'!
isValidStartOfIdentifiers
	"Can I be the first character of an identifier? (unary or keyword selector, or variable name)"

	^self isLetter or: [ $_ numericValue = value ]! !

!UnicodeCodePoint methodsFor: 'as yet unclassified' stamp: 'jmv 5/10/2022 15:43:13'!
doesNotUnderstand: aMessage

	{ 'UnicodeCodePoint does not understand: '. aMessage} print! !


!UnicodeCodePoint class methodsFor: 'instance creation' stamp: 'jmv 5/4/2022 16:53:51'!
codePoint: aNumber
	^self new codePoint: aNumber! !

