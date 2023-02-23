'From Cuis 6.0 [latest update: #5663] on 23 February 2023 at 4:12:02 pm'!

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 2/23/2023 13:38:50'!
at: codePointIndex put: aCharacterOrUnicodeCodePoint
	"Store argument at position codePointIndex "

	| byteIndex currentByteCount argumentBytes newBytes |

	self boundsCheck: codePointIndex.
	byteIndex _ self byteIndexAt: codePointIndex.
	currentByteCount _ self encodedByteCountAt: byteIndex.

	"Fastest path if both old and new are ASCII"
	currentByteCount = 1 ifTrue: [
		aCharacterOrUnicodeCodePoint asciiValue ifNotNil: [ :newAscii |
			bytes at: 	byteIndex put: newAscii.
			^ aCharacterOrUnicodeCodePoint ]].

	argumentBytes _ Character utf8BytesOfUnicodeCodePoint: aCharacterOrUnicodeCodePoint codePoint.

	"Avoid creating new bytes object if both old and new require same number of bytes"
	argumentBytes size = currentByteCount ifTrue: [
		bytes replaceFrom: byteIndex to: byteIndex + currentByteCount - 1 with: argumentBytes startingAt: 1.
		strideIndex _ nil.
		hash _ nil.
		^ aCharacterOrUnicodeCodePoint ].

	"More expensive: needs a new bytes object of different size"
	newBytes _ ByteArray new: bytes size - currentByteCount + argumentBytes size.
	newBytes
		replaceFrom: 1 to: byteIndex-1 with: bytes startingAt: 1;
		replaceFrom: byteIndex to: byteIndex + argumentBytes size - 1 with: argumentBytes startingAt: 1;
		replaceFrom: byteIndex + argumentBytes size to: newBytes size with: bytes startingAt: byteIndex + currentByteCount.

	self privateBytes: newBytes codePointCount: count.
	^ aCharacterOrUnicodeCodePoint.! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 2/23/2023 13:49:26'!
new: n
	"Answer will contain n null UnicodeCodePoints"

	^self fromUtf8Bytes: (ByteArray new: n) codePointCount: n! !


!UnicodeSymbol methodsFor: 'accessing' stamp: 'jmv 2/23/2023 16:08:32'!
at: anInteger put: anObject
	"you can not modify the receiver."

	self errorNoModification! !

!UnicodeSymbol methodsFor: 'accessing' stamp: 'jmv 2/23/2023 16:08:38'!
replaceFrom: start to: stop with: replacement startingAt: repStart

	self errorNoModification! !

!UnicodeSymbol methodsFor: 'private' stamp: 'jmv 2/23/2023 16:08:43'!
errorNoModification

	self error:  'symbols can not be modified.'! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 2/23/2023 13:39:04'!
at: codePointIndex
	"Answer the UnicodeCodePoint at code point index codePointIndex.
	(not a byte index!!)"

	| byteIndex |
	self boundsCheck: codePointIndex.
	self isAscii ifTrue: [ ^ UnicodeCodePoint codePoint: (bytes at: codePointIndex) ].
	byteIndex _ self byteIndexAt: codePointIndex.
	Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
		^UnicodeCodePoint codePoint: codePoint ].
	^nil "Invalid UTF-8"! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 2/23/2023 13:41:47'!
canonicalComposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	The canonical composition is cached. Therefor it doesn't need to be computed every time it is needed.
	This makes it actually cheaper than #canonicalDecomposition!!

	UnicodeString fromUtf8Bytes: #[195 162].
	(UnicodeString fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) canonicalComposition asUtf8Bytes

	UnicodeString fromUtf8Bytes: #[97 204 130].
	(UnicodeString fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[97 204 130]) canonicalComposition asUtf8Bytes
	"

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	^hash isNumber
		ifTrue: [
			"If 'hash' is a number it means we are already in canonicalComposition form (NFC)."
			self ]
		ifFalse: [
			"If 'hash' is not a number, it actually contains our #canonicalComposition.
			Answer a copy, because it could later be mutated."
			hash copy ].! !

!methodRemoval: UnicodeString #reversed stamp: 'jmv 2/23/2023 16:10:55'!
UnicodeString removeSelector: #reversed!
!methodRemoval: UnicodeString #shuffled stamp: 'jmv 2/23/2023 16:10:55'!
UnicodeString removeSelector: #shuffled!
!methodRemoval: UnicodeString #shallowCopy stamp: 'jmv 2/23/2023 16:10:55'!
UnicodeString removeSelector: #shallowCopy!

!String reorganize!
('accessing' at: at:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: replaceFrom:to:with:startingAt: size)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString: copyWith:)
('converting' asByteString asCodePoints asHex asLowercase asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('services' numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty isInCanonicalCompositionForm)
('paragraph support' encompassLine: encompassParagraph:)
!

