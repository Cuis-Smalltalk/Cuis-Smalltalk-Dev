'From Cuis 6.0 [latest update: #5145] on 11 May 2022 at 10:34:39 am'!
!classDefinition: #Utf32String category: #'System-Text'!
Object subclass: #Utf32String
	instanceVariableNames: 'codePoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Utf32String commentStamp: 'jmv 5/10/2022 15:47:34' prior: 0!
String like objects handling Unicode code points, encoded as UTF-32 bytes.

Install VectorGraphics / TrueType support, and play with the class side demo methods.!

!classDefinition: #Utf8String category: #'System-Text'!
Object subclass: #Utf8String
	instanceVariableNames: 'bytes count index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Text'!

!Utf8String commentStamp: 'jmv 5/10/2022 15:47:45' prior: 0!
String like objects handling Unicode code points, encoded as UTF-8 bytes.

Install VectorGraphics / TrueType support, and play with the class side demo methods.!


!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/5/2022 10:23:16'!
at: codePointIndex
	"Answer the UnicodeCodePoint at codePointIndex"

	^UnicodeCodePoint codePoint: (codePoints at: codePointIndex)! !

!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/11/2022 10:10:53'!
isLastLineSeparator
	"
	(Utf32String withBytes: #[64 65 66]) isLastLineSeparator
	(Utf32String withBytes: #[64 65 66 10]) isLastLineSeparator
	(Utf32String withBytes: #[64 65 66 13]) isLastLineSeparator
	(Utf32String withBytes: #[64 65 66 12]) isLastLineSeparator
	(Utf32String withBytes: #[64 65 66 194 133]) isLastLineSeparator
	(Utf32String withBytes: #[64 65 66 226 128 168 ]) isLastLineSeparator
	(Utf32String withBytes: #[64 65 66 226 128 169 ]) isLastLineSeparator
	"
	| s |
	s _ codePoints size.
	s = 0 ifTrue: [ ^false ].
	(#(10 13 12 16r0085 16r2028 16r2029) statePointsTo: 	(codePoints at: s)) ifTrue: [ ^true ].
	^false.
	! !

!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/11/2022 10:11:11'!
isLastSeparator
"A few more are missing!!"
	"
	(Utf32String withBytes: #[64 65 66]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 32]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 9]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 10]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 13]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 12]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 194 133]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 226 128 168 ]) isLastSeparator
	(Utf32String withBytes: #[64 65 66 226 128 169 ]) isLastSeparator
	"
	| s |
	s _ codePoints size.
	s = 0 ifTrue: [ ^false ].
	(#(32 9) statePointsTo: 	(codePoints at: s)) ifTrue: [ ^true ].
	self isLastLineSeparator ifTrue: [ ^true ].
	^false.
	! !

!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/4/2022 15:48:00'!
size
	^codePoints size! !

!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/4/2022 19:22:10'!
words
	^codePoints! !

!Utf32String methodsFor: 'accessing' stamp: 'jmv 5/5/2022 10:28:47'!
words: aWordArray
	codePoints _ aWordArray! !

!Utf32String methodsFor: 'copying' stamp: 'jmv 5/11/2022 10:02:48'!
, anUtf32String
	^Utf32String withWords: codePoints, anUtf32String words.! !

!Utf32String methodsFor: 'copying' stamp: 'jmv 5/11/2022 09:57:56'!
copyFrom: startIndex to: endIndex 
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."
	
	^Utf32String withWords:
		(codePoints
			copyFrom: startIndex
			to: endIndex)! !

!Utf32String methodsFor: 'copying' stamp: 'jmv 5/11/2022 09:57:56'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| epilogFirstIndex prologLastIndex |
	prologLastIndex _ startIndex-1.
	epilogFirstIndex _ endIndex+1.
	^Utf32String withWords:
		(codePoints copyFrom: 1 to: prologLastIndex),
		replacement asUtf32,
		(codePoints
			copyFrom: epilogFirstIndex
			to: codePoints size)! !

!Utf32String methodsFor: 'comparing' stamp: 'jmv 5/11/2022 10:07:38'!
= anUtf32String
	"Answer whether the receiver sorts equally as anUtf32String.
	This means same characters in same order."

	"Any object is equal to itself"
	self == anUtf32String ifTrue: [ ^ true ].

	"If argument is not a anUtf32String, maybe it is a Text?"
	anUtf32String class == Utf32String ifFalse: [
		(anUtf32String is: #Text) ifTrue: [ ^ self = anUtf32String string ].
		^ false].

	self size = anUtf32String size ifFalse: [
		^false ].

	self size > 256 ifTrue: [
		codePoints hashQuick = anUtf32String words hashQuick ifFalse: [ ^false ]].

	^ codePoints = anUtf32String words! !

!Utf32String methodsFor: 'comparing' stamp: 'jmv 5/5/2022 10:36:15'!
hash
	"Hash is reimplemented because = is implemented."

	^codePoints hash! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:16'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8: self asUtf8! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/4/2022 19:06:03'!
asPlainString
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:46'!
asString
	"Answer the receiver itself."
	^self! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/4/2022 15:19:36'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:28'!
asUtf32
	"Answer a WordArray."

	^codePoints! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:07:15'!
asUtf32: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-32 doesn't need them after all.
	Answer a WordArray."

	^codePoints! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:03:25'!
asUtf8
	"Answer a ByteArray."

	^String utf8FromUtf32: codePoints! !

!Utf32String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:03'!
asUtf8: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-32 doesn't need them after all.
	Answer a ByteArray."

	^self asUtf8! !

!Utf32String methodsFor: 'enumerating' stamp: 'jmv 5/5/2022 11:25:41'!
from: start to: stop do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	start to: stop do: [ :index | aBlock value: (self at: index) ].! !

!Utf32String methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:28:36'!
is: aSymbol
{ #is: . self class. aSymbol} print.
	^ super is: aSymbol! !

!Utf32String methodsFor: 'testing' stamp: 'jmv 5/4/2022 15:19:54'!
isEmpty
	^codePoints isEmpty! !

!Utf32String methodsFor: 'testing' stamp: 'jmv 5/4/2022 15:19:36'!
isStringLike

	^true! !

!Utf32String methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 16:56:56'!
encompassLine: anInterval
	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.
	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)
	See also encompassParagraph:"
	| left rightCr rightNull |
	left _ (codePoints lastIndexOf: `Character newLineCharacter numericValue` startingAt: anInterval first - 1 ifAbsent:[0]) + 1.
	rightCr _ (codePoints indexOf: `Character newLineCharacter numericValue` startingAt: (anInterval last max: anInterval first) ifAbsent: [codePoints size+1])-1.
	rightNull _ (codePoints indexOf: 0 startingAt: (anInterval last max: anInterval first) ifAbsent: [codePoints size+1])-1.
	^left to: (rightCr min: rightNull)! !

!Utf32String methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 15:32:57'!
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

!Utf32String methodsFor: 'character scanner support' stamp: 'jmv 5/11/2022 10:05:52'!
scanCharactersFrom: startIndex to: stopIndex with: aCharacterScanner rightX: rightX stopConditions: stops
	"Double dispatch."

	^ aCharacterScanner scanUtf32StringFrom: startIndex to: stopIndex in: self rightX: rightX stopConditions: stops! !

!Utf32String methodsFor: 'user interface' stamp: 'jmv 5/11/2022 10:15:37'!
edit

	self editLabel: 'UTF-32 Text Editor'! !

!Utf32String methodsFor: 'user interface' stamp: 'jmv 5/11/2022 10:20:28'!
editLabel: labelString

	(TextModel withText: self) openLabel: labelString! !

!Utf32String methodsFor: 'as yet unclassified' stamp: 'jmv 5/11/2022 10:05:22'!
doesNotUnderstand: aMessage

	{ 'Utf32String does not understand: '. aMessage} print! !


!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:19:26'!
fromUtf32: words

	^self withWords: words! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:15:48'!
fromUtf8: aByteArray

	^self withWords: (String utf32FromUtf8: aByteArray).! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/11/2022 10:06:15'!
with: aCharacter 
	"Answer a new instance of me, containing utf words for aCharacter.
	Utf32String with: $a
	Utf32String with: $á
	Utf32String with: Character euro
	"
	^self withWords: (WordArray with: aCharacter codePoint).! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/11/2022 10:06:33'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf words for aCharacter.
	Utf32String with: $a
	Utf32String with: $á with: Character euro
	"
	^self withWords: (WordArray with: aCharacter codePoint with: otherCharacter codePoint).! !

!Utf32String class methodsFor: 'instance creation' stamp: 'jmv 5/4/2022 15:49:33'!
withWords: aWordArray
	^self new words: aWordArray ! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/6/2022 18:48:36'!
at: codePointIndex
	"Answer the UnicodeCodePoint at code point index codePointIndex (not a byte index!!)"

	| byteIndex |
	self boundsCheck: codePointIndex.
	self isAscii ifTrue: [ ^ UnicodeCodePoint codePoint: (bytes at: codePointIndex) ].
	byteIndex _ self byteIndexAt: codePointIndex.
	Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
		^UnicodeCodePoint codePoint: codePoint ].! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/4/2022 19:22:50'!
bytes
	^bytes! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/5/2022 10:23:51'!
bytes: aByteArray
	bytes _ aByteArray! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/11/2022 10:10:20'!
isLastLineSeparator
	"
	(Utf8String withBytes: #[64 65 66]) isLastLineSeparator
	(Utf8String withBytes: #[64 65 66 10]) isLastLineSeparator
	(Utf8String withBytes: #[64 65 66 13]) isLastLineSeparator
	(Utf8String withBytes: #[64 65 66 12]) isLastLineSeparator
	(Utf8String withBytes: #[64 65 66 194 133]) isLastLineSeparator
	(Utf8String withBytes: #[64 65 66 226 128 168 ]) isLastLineSeparator
	(Utf8String withBytes: #[64 65 66 226 128 169 ]) isLastLineSeparator
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

!Utf8String methodsFor: 'accessing' stamp: 'jmv 5/11/2022 10:10:34'!
isLastSeparator
"A few more are missing!!"
	"
	(Utf8String withBytes: #[64 65 66]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 32]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 9]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 10]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 13]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 12]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 194 133]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 226 128 168 ]) isLastSeparator
	(Utf8String withBytes: #[64 65 66 226 128 169 ]) isLastSeparator
	"
	| s |
	s _ bytes size.
	s = 0 ifTrue: [ ^false ].
	(#(32 9) statePointsTo: 	(bytes at: s)) ifTrue: [ ^true ].
	self isLastLineSeparator ifTrue: [ ^true ].
	^false.
	! !

!Utf8String methodsFor: 'accessing' stamp: 'svc 5/5/2022 15:51:50'!
size
	"Return how many characters I contain"
	
	self isEmpty ifTrue: [ ^ 0 ].
	count ifNil: [ self computeCountAndIndex ].
	^ count! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 5/11/2022 10:07:03'!
, anUtf8String
	^Utf8String withBytes: bytes, anUtf8String bytes.! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 5/11/2022 09:58:18'!
copyFrom: startIndex to: stopIndex
	"Return a new string, a copy of me from startIndex to stopIndex, both character indices.
	Signal SubscriptOutOfBounds or CollectionIsEmpty in case of illegal indices."
	
	| startByteIndex stopByteIndex substring |
	startIndex = (self size+1) ifTrue: [ ^Utf8String empty ].
	stopIndex = 0 ifTrue: [ ^Utf8String empty ].
	self boundsCheck: startIndex. 
	self boundsCheck: stopIndex. 
	startByteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	stopByteIndex := stopByteIndex + (self encodedByteCountAt: stopByteIndex) - 1.
	substring := Utf8String withBytes: (bytes copyFrom: startByteIndex to: stopByteIndex).
	self isAscii ifTrue: [ substring beAscii ].
	^ substring! !

!Utf8String methodsFor: 'copying' stamp: 'jmv 5/11/2022 09:58:18'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| epilogFirstIndex prologLastIndex |
	prologLastIndex _ (self byteIndexAt: startIndex)-1.
	epilogFirstIndex _ self byteIndexAt: endIndex+1.
	^Utf8String withBytes:
		(bytes copyFrom: 1 to: prologLastIndex),
		replacement asUtf8,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size)! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 5/11/2022 10:08:06'!
= anUtf8String
	"Answer whether the receiver sorts equally as anUtf8String.
	This means same characters in same order."

	"Any object is equal to itself"
	self == anUtf8String ifTrue: [ ^ true ].

	"If argument is not a anUtf32String, maybe it is a Text?"
	anUtf8String class == Utf8String ifFalse: [
		(anUtf8String is: #Text) ifTrue: [ ^ self = anUtf8String string ].
		^ false].

	bytes size = anUtf8String bytes size ifFalse: [
		^false ].

	bytes size > 256 ifTrue: [
		bytes hashQuick = anUtf8String bytes hashQuick ifFalse: [ ^false ]].

	^ bytes = anUtf8String bytes! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 5/5/2022 10:38:57'!
hash
	"Hash is reimplemented because = is implemented."

	^bytes hash! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:03'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8: bytes! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/4/2022 19:06:11'!
asPlainString
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/5/2022 10:13:44'!
asString
	"Answer the receiver itself."
	^self! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 3/13/2022 18:50:31'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:39'!
asUtf32
	"Answer a WordArray."

	^String utf32FromUtf8: bytes! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:07:29'!
asUtf32: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-32 doesn't need them after all.
	Answer a WordArray."

	^self asUtf32! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:03:32'!
asUtf8
	"Answer a ByteArray."

	^bytes! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 5/6/2022 19:04:10'!
asUtf8: convertEmbeddedNCRs
	"Assume that only String will contain NCRs. UTF-8 doesn't need them after all.
	Answer a ByteArray."

	^bytes! !

!Utf8String methodsFor: 'enumerating' stamp: 'jmv 5/5/2022 16:57:33'!
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

!Utf8String methodsFor: 'enumerating' stamp: 'jmv 5/5/2022 16:57:37'!
from: startIndex to: stopIndex doWithUtf8Bytes: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	| byteIndex n stopByteIndex |
	byteIndex _ self byteIndexAt: startIndex.
	stopByteIndex _ self byteIndexAt: stopIndex.
	[ byteIndex <= stopByteIndex ] whileTrue: [
		n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: aBlock into: nil.
		byteIndex _ byteIndex + n ].! !

!Utf8String methodsFor: 'testing' stamp: 'jmv 5/5/2022 15:28:32'!
is: aSymbol
{ #is: . self class. aSymbol} print.
	^ super is: aSymbol! !

!Utf8String methodsFor: 'testing' stamp: 'svc 5/5/2022 16:12:43'!
isAscii
	"Return true when I am pure 7-bit ASCII, which means each character is encoded as one byte."

	count ifNil: [ self computeCountAndIndex ].
	^ bytes size = count! !

!Utf8String methodsFor: 'testing' stamp: 'jmv 5/3/2022 12:12:52'!
isEmpty
	^bytes isEmpty! !

!Utf8String methodsFor: 'testing' stamp: 'jmv 4/29/2022 11:35:03'!
isStringLike

	^true! !

!Utf8String methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 16:57:00'!
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

!Utf8String methodsFor: 'paragraph support' stamp: 'jmv 5/5/2022 16:57:40'!
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

!Utf8String methodsFor: 'character scanner support' stamp: 'jmv 5/11/2022 10:07:59'!
scanCharactersFrom: startIndex to: stopIndex with: aCharacterScanner rightX: rightX stopConditions: stops
	"Double dispatch."

	^ aCharacterScanner scanUtf8StringFrom: startIndex to: stopIndex in: self rightX: rightX stopConditions: stops! !

!Utf8String methodsFor: 'user interface' stamp: 'jmv 5/11/2022 10:16:02'!
edit

	self editLabel: 'UTF-8 Text Editor'! !

!Utf8String methodsFor: 'user interface' stamp: 'jmv 5/11/2022 10:20:31'!
editLabel: labelString

	(TextModel withText: self) openLabel: labelString! !

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 15:53:18'!
beAscii
	"Mark me as being pure 7-bit ASCII, which means each character is encoded as one byte"
	
	count := bytes size.
	index := #().! !

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 16:04:35'!
boundsCheck: codePointIndex
	(codePointIndex between: 1 and: self size) 
		ifFalse: [ self errorSubscriptBounds: codePointIndex ]! !

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 16:25:57'!
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

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 16:46:33'!
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

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 15:45:39'!
computeCountAndIndex
	self isEmpty ifTrue: [ count := 0. ^ self ].
	"the number of characters cannot be larger than the byte size"
	bytes size <= self stride
		ifTrue: [ self computeCountNoIndex ]
		ifFalse: [ self computeCountAndIndexFull ]! !

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 15:46:55'!
computeCountAndIndexFull
	count := 0.
	index := Array streamContents: [ :out | | byteIndex |
		byteIndex := 1.
		[ byteIndex <= bytes size ] whileTrue: [
			count := count + 1.
			(count > self stride and: [ (count - 1 \\ self stride) = 0 ]) 
				ifTrue: [ out nextPut: byteIndex ].
			byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ] ]! !

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 15:46:18'!
computeCountNoIndex
	| byteIndex |
	count := 0.
	index := #().
	byteIndex := 1.
	[ byteIndex <= bytes size ] whileTrue: [
		count := count + 1.
		byteIndex := byteIndex + (self encodedByteCountAt: byteIndex) ]! !

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 15:50:23'!
encodedByteCountAt: byteIndex
	| byte |
	(byte := bytes at: byteIndex) < 128 ifTrue: [ ^ 1 ].
	(byte bitAnd: 2r11100000) = 2r11000000 ifTrue: [ ^ 2 ].
	(byte bitAnd: 2r11110000) = 2r11100000 ifTrue: [ ^ 3 ].
	(byte bitAnd: 2r11111000) = 2r11110000 ifTrue: [ ^ 4 ].
	self error: 'IllegalLeadingByte '! !

!Utf8String methodsFor: 'private' stamp: 'svc 5/5/2022 15:45:54'!
stride
	"Return the stride, the constant I use internally to build my index"
	
	^ 32! !

!Utf8String methodsFor: 'as yet unclassified' stamp: 'jmv 5/11/2022 10:10:04'!
doesNotUnderstand: aMessage

	{ 'Utf8String does not understand: '. aMessage} print! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/11/2022 10:10:07'!
empty
	"Return the empty Utf8String instance.
	Since my instances are readonly, this could be cached."

	^ (self withBytes: #[]) beAscii; yourself! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:16:43'!
fromUtf32: aWordArray

	^self withBytes: (String utf8FromUtf32: aWordArray).! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/5/2022 10:19:21'!
fromUtf8: bytes

	^self withBytes: bytes! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/11/2022 10:11:25'!
with: aCharacter 
	"Answer a new instance of me, containing utf bytes for aCharacter.
	Utf8String with: $a
	Utf8String with: $á
	Utf8String with: Character euro
	"
	^self withBytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint).! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/11/2022 10:11:36'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter.
	Utf8String with: $a
	Utf8String with: $á with: Character euro
	"
	^self withBytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint),
		(Character utf8BytesOfUnicodeCodePoint: otherCharacter codePoint).! !

!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 5/3/2022 12:10:29'!
withBytes: aByteArray
	^self new bytes: aByteArray ! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 5/11/2022 10:05:41'!
scanUtf32StringFrom: startIndex to: stopIndex in: anUtf32String rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX |
	lastIndex _ startIndex.
	anUtf32String words from: startIndex to: stopIndex do: [ :code |
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

!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 5/11/2022 10:08:19'!
scanUtf8StringFrom: startIndex to: stopIndex in: anUtf8String rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX |
	lastIndex _ startIndex.
	anUtf8String from: startIndex to: stopIndex doWithUtf8Bytes: [ :byte1 :byte2 :byte3 :byte4 |
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


!Object methodsFor: 'testing' stamp: 'jmv 5/11/2022 10:08:57'!
isStringLike
	"Overridden to return true in String, Utf8String and Utf32String."
	^ false! !


!String methodsFor: 'user interface' stamp: 'jmv 5/11/2022 10:20:22'!
editLabel: labelString

	(TextModel withText: self) openLabel: labelString! !


!Text methodsFor: 'user interface' stamp: 'jmv 5/11/2022 10:20:26'!
editLabel: labelString

	(TextModel withText: self) openLabel: labelString! !

!methodRemoval: CharacterScanner #scanStringUtf8From:to:in:rightX:stopConditions: stamp: 'jmv 5/11/2022 10:34:01'!
CharacterScanner removeSelector: #scanStringUtf8From:to:in:rightX:stopConditions:!
!methodRemoval: CharacterScanner #scanStringUtf32From:to:in:rightX:stopConditions: stamp: 'jmv 5/11/2022 10:34:01'!
CharacterScanner removeSelector: #scanStringUtf32From:to:in:rightX:stopConditions:!
!classRemoval: #StringUtf32 stamp: 'jmv 5/11/2022 10:33:50'!
Smalltalk removeClassNamed: #StringUtf32!

!classRemoval: #StringUtf8 stamp: 'jmv 5/11/2022 10:33:53'!
Smalltalk removeClassNamed: #StringUtf8!

