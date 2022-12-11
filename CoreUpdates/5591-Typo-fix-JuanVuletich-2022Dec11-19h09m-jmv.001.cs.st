'From Cuis 6.0 [latest update: #5590] on 11 December 2022 at 7:15:43 pm'!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value '
	classVariableNames: 'CannonicalCombiningClasses CompositionMapping DecompositionMapping DiacriticalMarksTable LowercaseLettersTable NamedCharactersMap SymbolsLikeTable UppercaseLettersTable CanonicalCombiningClasses '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/11/2022 19:05:09'!
canonicalCompositionOf: numericCodePoint and: combiningCodePoint

	| dict |
	dict _ CompositionMapping at: numericCodePoint
		ifAbsent: [^nil ].
	^dict at: combiningCodePoint ifAbsent: nil! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/11/2022 19:06:32'!
canonicalDecomposition: numericCodePoint on: aStream

	| decomposition |
	decomposition _ DecompositionMapping at: numericCodePoint
		ifAbsent: [^aStream nextPutCodePoint: numericCodePoint].
	decomposition do: [ :each |
		self canonicalDecomposition: each on: aStream ]! !


!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:05:00'!
canonicalComposition
	^self! !

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:05:14'!
canonicalDecomposition
	^self! !


!String methodsFor: 'testing' stamp: 'jmv 12/11/2022 19:05:30'!
isInCanonicalCompositionForm

	^true! !


!UnicodeString methodsFor: 'testing' stamp: 'jmv 12/11/2022 19:13:20'!
isInCanonicalCompositionForm
	"Answer if we are in NFC"

	self isAscii ifTrue: [ ^true ].

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	"See findCanonicalCompositionAndHash"
	^hash isNumber! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:11:42'!
basicCanonicalComposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalComposition.
	(UnicodeString fromCodePoints: #(119128 119141)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119135 119151)) basicCanonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119128 119141 119151)) basicCanonicalComposition asCodePoints
	"
	| byteIndex stopByteIndex n combining cls |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	combining _ OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls _ UnicodeCodePoint combiningClass: codePoint.
				cls = 0 ifTrue: [
					self basicCompose: combining on: strm.
					combining _ OrderedCollection new ].
				combining add: codePoint ].
			byteIndex _ byteIndex + n ].
		self basicCompose: combining on: strm ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:11:54'!
basicCanonicalDecomposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalDecomposition.
	(UnicodeCodePoint codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex n |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				UnicodeCodePoint canonicalDecomposition: codePoint on: strm ].
			byteIndex _ byteIndex + n ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:12:10'!
basicCanonicallyOrdered
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(UnicodeString fromCodePoints: #(16r61  16r0300 16r0315 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0300 16r031B 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r031B 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r0300 16r031B)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0300 16r0315)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0315 16r0300)) basicCanonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	"
	| byteIndex stopByteIndex n combining cls element |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	combining _ OrderedCollection new.
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls _ UnicodeCodePoint combiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
							combining do: [ :e | strm nextPutCodePoint: e third ]].
						strm nextPutCodePoint: codePoint.
						combining _ OrderedCollection new ]
					ifFalse: [
						element _ { cls. combining size+1. codePoint }.
						combining add: element ]].
			byteIndex _ byteIndex + n ].
		combining notEmpty ifTrue: [
			combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
			combining do: [ :e | strm nextPutCodePoint: e third ] ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:12:35'!
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
			"If 'hash' is not a number, it actually contains out #canonicalComposition."
			hash ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:12:51'!
canonicalDecomposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(UnicodeCodePoint codePoint: 16r1EBF) asString
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCanonicalDecomposition asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]).
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeCodePoint codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	"
	^self basicCanonicalDecomposition basicCanonicallyOrdered! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:13:48'!
findCanonicalCompositionAndHash
	"Find our canonical composition (NFC). If we are found to be in NFC, store our hash.
	Otherwise, store our NFC. Queries of #hash will be forwarded to it."

	| canonicalComposition |
	"Ascii is already normalized to NFC."
	self isAscii ifTrue: [
		^self computeHash ].

	"Find our canonical composition"
	canonicalComposition _ self basicCanonicalDecomposition basicCanonicallyOrdered basicCanonicalComposition.
	(String is: bytes equalTo: canonicalComposition bytes)
		ifTrue: [
			"If we were already in NFC, compute and store our hash"
			self computeHash ]
		ifFalse: [
			"Otherwise, point to our NFC, and ensure it has its hash, for us to query when needed."
			hash _ canonicalComposition.
			canonicalComposition computeHash ].! !


!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 12/11/2022 19:14:56'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory canonicalCombiningClass characterDecompositionMapping decomposition composition |
	UppercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	CanonicalCombiningClasses _ Dictionary new.
	DecompositionMapping _ Dictionary new.
	CompositionMapping _ Dictionary new.
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields _ strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self halt ].
			codePoint _ ('16r', fields first) asNumber.
			codePointName _ fields at: 2.
			generalCategory _ fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				({ 'Lu'. 'Lt' } includes: generalCategory) ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Ll'. 'Lo' } includes: generalCategory) ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Pc'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Mn'. 'Mc'. 'Me' } includes: generalCategory) ifTrue: [ DiacriticalMarksTable bitBooleanAt: codePoint+1 put: true ]].
			canonicalCombiningClass _ (fields at: 4) asNumber.
			canonicalCombiningClass = 0 ifFalse: [
				CanonicalCombiningClasses at: codePoint put: canonicalCombiningClass ].
			characterDecompositionMapping _ fields at: 6.
			(characterDecompositionMapping size > 0 and: [ characterDecompositionMapping first ~= $< ]) ifTrue: [
				decomposition _ characterDecompositionMapping substrings collect: [ :s | ('16r', s) asNumber].
				DecompositionMapping at: codePoint put: decomposition.
				decomposition size > 1 ifTrue: [
					composition _ CompositionMapping at: decomposition first ifAbsentPut: [Dictionary new].
					composition at: decomposition second put: codePoint.
					decomposition size > 2 ifTrue: [decomposition print. self halt].
					]].
			"
			uppercaseMapping _ fields at: 13. lowercaseMapping _ fields at: 14. titlecaseMapping _ fields at: 15.
			Random next > 10.999 | (generalCategory = 'So') ifTrue: [
				{codePoint hex. codePointName. generalCategory} print ].
			decimalDigitValue _ fields at: 7. digitValue _ fields at: 8. numericValue _ fields at: 9.
			decimalDigitValue isEmpty & digitValue isEmpty & numericValue isEmpty ifFalse: [
				{'numeric'. codePoint. codePointName. generalCategory. decimalDigitValue. digitValue. numericValue} print].
			"
			]].! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/11/2022 19:14:16'!
combiningClass: numericCodePoint
	(self isDiacriticalMark: numericCodePoint) ifFalse: [ ^0 ].
	^CanonicalCombiningClasses at: numericCodePoint ifAbsent: [ 	0 ]! !


!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/11/2022 19:07:12'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, UnicodeString, UnicodeSymbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self hash = aString hash ifFalse: [
		^false ].

	(self isInCanonicalCompositionForm and: [ aString isInCanonicalCompositionForm ]) ifFalse: [
		^self canonicalComposition = aString canonicalComposition ].

	"Now we can assume we are both in NFC"
	self size = aString size ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [	
		"Fast lane."
		^String is: bytes equalTo: aString ].	

	aString isUnicodeString ifFalse: [
		^ self beginsWith: aString ].

	self isAscii = aString isAscii ifFalse: [
		^false ].

	^String is: bytes equalTo: aString bytes.! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/11/2022 19:12:16'!
basicHash
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859 non ASCII characters are present.
	Note: will answer different values for UnicodeString's  that are equal if #canonicalDecomposition is taken."
	bytes size >  ByteArray minSizeForHashQuick ifTrue: [
		^ bytes hashQuick: String hash ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/11/2022 19:13:16'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCanonicalCompositionAndHash ].

	"If 'hash' is not a number, it actually contains out #canonicalComposition. Ask for its hash then."
	hash isNumber ifFalse: [
		^hash hash ].

	"If 'hash' is a number it means we are in canonicalComposition form (NFC), and 'hash' is an good hash value."
	^hash! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:07:20'!
asNFC
	"Convenience"
	^self canonicalComposition! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:06:50'!
asNFD
	"Convenience"
	^self canonicalDecomposition! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/11/2022 19:07:00'!
basicCompose: combiningCodePoints on: aStream
	| continue composition i base |
	combiningCodePoints notEmpty ifTrue: [
		continue _ combiningCodePoints size > 1.
		[continue] whileTrue: [
			continue _ false.
			base _ combiningCodePoints first.
			i _ 2.
			composition _ nil.
			[composition isNil and: [i <= combiningCodePoints size ]] whileTrue: [
				composition _ UnicodeCodePoint canonicalCompositionOf: base and: (combiningCodePoints at: i).
				composition ifNotNil: [
					combiningCodePoints at: 1 put: composition.
					combiningCodePoints removeAt: i.
					continue _ combiningCodePoints size > 1 ].
				i _ i + 1 ]].
		combiningCodePoints do: [ :e | aStream nextPutCodePoint: e ]].! !


!UnicodeSymbol class methodsFor: 'private' stamp: 'jmv 12/11/2022 19:13:30'!
basicFromString: aString
	"Private use. Answers an instance that is not yet interned.
	Can break the Symbol invariant: forAllSymbols, (s1 = s2) => (s1 == s2)
	Please don't call lightly."

	| canonicalComposition utf8Bytes |
	canonicalComposition _ aString canonicalComposition.
	utf8Bytes _ canonicalComposition asUtf8Bytes.
	^self basicNew privateBytes: utf8Bytes codePointCount: canonicalComposition size.! !

!methodRemoval: UnicodeString #basicCannonicalDecomposition stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeString removeSelector: #basicCannonicalDecomposition!
!methodRemoval: UnicodeString #basicCannonicallyOrdered stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeString removeSelector: #basicCannonicallyOrdered!
!methodRemoval: UnicodeString #isInCannonicalCompositionForm stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeString removeSelector: #isInCannonicalCompositionForm!
!methodRemoval: UnicodeString #cannonicalDecomposition stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeString removeSelector: #cannonicalDecomposition!
!methodRemoval: UnicodeString #cannonicalComposition stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeString removeSelector: #cannonicalComposition!
!methodRemoval: UnicodeString #findCannonicalCompositionAndHash stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeString removeSelector: #findCannonicalCompositionAndHash!
!methodRemoval: UnicodeString #basicCannonicalComposition stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeString removeSelector: #basicCannonicalComposition!
!methodRemoval: String #isInCannonicalCompositionForm stamp: 'jmv 12/11/2022 19:10:36'!
String removeSelector: #isInCannonicalCompositionForm!
!methodRemoval: CharacterSequence #cannonicalDecomposition stamp: 'jmv 12/11/2022 19:10:36'!
CharacterSequence removeSelector: #cannonicalDecomposition!
!methodRemoval: CharacterSequence #cannonicalComposition stamp: 'jmv 12/11/2022 19:10:36'!
CharacterSequence removeSelector: #cannonicalComposition!
!methodRemoval: UnicodeCodePoint class #cannonicalDecomposition:on: stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeCodePoint class removeSelector: #cannonicalDecomposition:on:!
!methodRemoval: UnicodeCodePoint class #cannonicalCompositionOf:and: stamp: 'jmv 12/11/2022 19:10:36'!
UnicodeCodePoint class removeSelector: #cannonicalCompositionOf:and:!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: 'CanonicalCombiningClasses CompositionMapping DecompositionMapping DiacriticalMarksTable LowercaseLettersTable NamedCharactersMap SymbolsLikeTable UppercaseLettersTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!
UnicodeCodePoint initialize!
