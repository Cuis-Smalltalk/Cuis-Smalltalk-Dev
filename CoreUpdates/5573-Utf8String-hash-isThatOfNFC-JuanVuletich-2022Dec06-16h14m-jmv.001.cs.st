'From Cuis 6.0 [latest update: #5572] on 6 December 2022 at 4:56:02 pm'!

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 16:45:29'!
findCannonicalCompositionAndHash
	"Find our cannonical composition (NFC). If we are found to be in NFC, store our hash.
	Otherwise, store our NFC. Queries of #hash will be forwarded to it."

	| byteIndex noDiacriticalFound stopByteIndex cannonicalComposition |
	"Ascii is already normalized to NFC."
	self isAscii ifTrue: [
		^self computeHash ].

	"Do a search for combining code points. If none is found, answer self."
	noDiacriticalFound _ true.
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	[ noDiacriticalFound and: [byteIndex <= stopByteIndex] ] whileTrue: [ | n |
		n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			(UnicodeCodePoint isDiacriticalMark: codePoint)
				ifTrue: [noDiacriticalFound _ false]].
		byteIndex _ byteIndex + n ].
	"No diacriticals? We are already normalized to NFC."
	noDiacriticalFound ifTrue: [
		^self computeHash ].

	"Find our cannonical composition"
	cannonicalComposition _ self basicCannonicalDecomposition basicCannonicallyOrdered basicCannonicalComposition.
	(String is: bytes equalTo: cannonicalComposition bytes)
		ifTrue: [
			"If we were already in NFC, compute and store our hash"
			self computeHash ]
		ifFalse: [
			"Otherwise, point to our NFC, and ensure it has its hash, for us to query when needed."
			hash _ cannonicalComposition.
			cannonicalComposition computeHash ].! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 15:26:22'!
basicHash
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859 non ASCII characters are present.
	Note: will answer different values for Utf8String's  that are equal if #cannonicalDecomposition is taken."
	bytes size >  ByteArray minSizeForHashQuick ifTrue: [
		^ bytes hashQuick: String hash ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 16:40:39'!
computeHash
"In the end, no need to answer hash."
	"Same hash value as the equivalent ASCII String, provided that all our CodePoints exist as Characters"

	| byteString |
	self isAscii ifTrue: [
		hash _ self basicHash.
		^hash ].

	"If there's a String that is equal to us (i.e. no characters were lost during conversion)"
	byteString _ self asByteString.
	byteString size = self size ifTrue: [
		hash _ byteString hash.
		^hash ].

	"Otherwise, use a hash that doesn't need to equal that of a String"
	hash _ self basicHash.
	^hash! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 16:50:00'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, Utf8String, Utf8Symbol, Utf32String and Text
	if they hold same Characters."

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCannonicalCompositionAndHash ].

	"If 'hash' is not a number, it actually contains out #cannonicalComposition. Ask for its hash then."
	hash isNumber ifFalse: [
		^hash hash ].

	"If 'hash' is a number it means we are in cannonicalComposition form (NFC), and 'hash' is an good hash value."
	^hash! !

!Utf8String methodsFor: 'private' stamp: 'jmv 12/6/2022 16:04:56'!
privateBytes: utf8ByteArray codePointCount: aNumberOrNil
	"Initialize the receiver. If we already know that utf8ByteArray only holds bytes less than 128 (i.e. ASCII characters), initialize 'count'. This is an important optimization."

	bytes _ utf8ByteArray.
	count _ aNumberOrNil.
	strideIndex _ nil.
	hash _ nil.! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 16:55:03'!
cannonicalComposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	The cannonical composition is cached. Therefor it doesn't need to be computed every time it is needed.
	This makes it actually cheaper than #cannonicalDecomposition!!

	Utf8String fromUtf8Bytes: #[195 162].
	(Utf8String fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[195 162]) cannonicalDecomposition asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[195 162]) cannonicalComposition asUtf8Bytes

	Utf8String fromUtf8Bytes: #[97 204 130].
	(Utf8String fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[97 204 130]) cannonicalDecomposition asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[97 204 130]) cannonicalComposition asUtf8Bytes
	"

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCannonicalCompositionAndHash ].

	^hash isNumber
		ifTrue: [
			"If 'hash' is a number it means we are already in cannonicalComposition form (NFC)."
			self ]
		ifFalse: [
			"If 'hash' is not a number, it actually contains out #cannonicalComposition."
			hash ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Utf8String allInstancesDo: [ :s | s instVarNamed: 'hash' put: nil ].!

