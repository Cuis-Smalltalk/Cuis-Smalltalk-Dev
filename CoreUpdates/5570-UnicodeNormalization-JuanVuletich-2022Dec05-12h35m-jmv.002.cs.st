'From Cuis 6.0 [latest update: #5569] on 5 December 2022 at 12:38:55 pm'!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value '
	classVariableNames: 'DiacriticalMarksTable LowercaseLettersTable NamedCharactersMap SymbolsLikeTable UppercaseLettersTable CannonicalCombiningClasses DecompositionMapping CompositionMapping '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/2/2022 16:19:00'!
cannonicalCompositionOf: numericCodePoint and: combiningCodePoint
	"
	"
	| dict |
	dict _ CompositionMapping at: numericCodePoint
		ifAbsent: [^nil ].
	^dict at: combiningCodePoint ifAbsent: nil! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/2/2022 15:53:18'!
cannonicalDecomposition: numericCodePoint on: aStream
	"
	$a cannonicalDecomposition
	UnicodeCodePoint cannonicalDecomposition: $a codePoint
	$á cannonicalDecomposition
	UnicodeCodePoint cannonicalDecomposition: $á codePoint
	"
	| decomposition |
	decomposition _ DecompositionMapping at: numericCodePoint
		ifAbsent: [^aStream nextPutCodePoint: numericCodePoint].
	decomposition do: [ :each |
		self cannonicalDecomposition: each on: aStream ]! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/2/2022 14:14:40'!
combiningClass: numericCodePoint
	(self isDiacriticalMark: numericCodePoint) ifFalse: [ ^0 ].
	^CannonicalCombiningClasses at: numericCodePoint! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/2/2022 14:15:14'!
isCombiningStarter: numericCodePoint
	^ (self combiningClass: numericCodePoint) = 0! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/2/2022 14:13:46'!
isDiacriticalMark: numericCodePoint
	"
	(UnicodeCodePoint isDiacriticalMark: 16r0041) 
	(UnicodeCodePoint codePoint: 16r0041) isDiacriticalMark
	(UnicodeCodePoint isDiacriticalMark: 16r20D7)
	(UnicodeCodePoint codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i _ numericCodePoint + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.! !


!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/5/2022 11:47:46'!
cannonicalComposition
	"
	Usually prefer #composedNormalization or #decomposedNormalization
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #composedNormalization.
	(Utf8String codePoints: #(119128 119141)) cannonicalComposition asUtf32Words asArray
	(Utf8String codePoints: #(119135 119151)) cannonicalComposition asUtf32Words asArray
	(Utf8String codePoints: #(119128 119141 119151)) cannonicalComposition asUtf32Words asArray
	"
	| byteIndex stopByteIndex n combining cls composition continue |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	combining _ OrderedCollection new.
	^Utf8String streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls _ UnicodeCodePoint combiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							continue _ combining size > 1.
							[continue] whileTrue: [
								continue _ false.
								composition _ UnicodeCodePoint cannonicalCompositionOf: combining first and: combining second.
								composition ifNotNil: [
									combining removeFirst.
									combining at: 1 put: composition.
									continue _ combining size > 1. ]
								].
							combining do: [ :e | strm nextPutCodePoint: e ]].
						combining _ OrderedCollection new ].
				combining add: codePoint ].
			byteIndex _ byteIndex + n ].
		combining notEmpty ifTrue: [
			continue _ combining size > 1.
			[continue] whileTrue: [
				continue _ false.
				composition _ UnicodeCodePoint cannonicalCompositionOf: combining first and: combining second.
				composition ifNotNil: [
					combining removeFirst.
					combining at: 1 put: composition.
					continue _ combining size > 1. ]
				].
			combining do: [ :e | strm nextPutCodePoint: e ]]].! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/5/2022 11:34:05'!
cannonicalDecomposition
	"
	Usually prefer #composedNormalization or #decomposedNormalization
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #decomposedNormalization.
	(UnicodeCodePoint codePoint: 119135) asString cannonicalDecomposition asUtf32Words asArray
	(UnicodeCodePoint codePoint: 119137) asString cannonicalDecomposition asUtf32Words asArray
	"
	| byteIndex stopByteIndex n |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	^Utf8String streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				UnicodeCodePoint cannonicalDecomposition: codePoint on: strm ].
			byteIndex _ byteIndex + n ]].! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/5/2022 11:48:26'!
cannonicallyOrdered
	"
	Usually prefer #composedNormalization or #decomposedNormalization
	https://unicode.org/reports/tr15/#Norm_Forms

	(Utf8String codePoints: #(16r61  16r0300 16r0315 16r031B)) cannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r0300 16r031B 16r0315)) cannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r0315 16r031B 16r0300)) cannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r0315 16r0300 16r031B)) cannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r031B 16r0300 16r0315)) cannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r031B 16r0315 16r0300)) cannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	"
	| byteIndex stopByteIndex n combining cls element |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	combining _ OrderedCollection new.
	^Utf8String streamContents: [ :strm |
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

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/5/2022 12:33:26'!
composedNormalization
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	Utf8String fromUtf8Bytes: #[195 162].
	(Utf8String fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[195 162]) decomposedNormalization asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[195 162]) composedNormalization asUtf8Bytes

	Utf8String fromUtf8Bytes: #[97 204 130].
	(Utf8String fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[97 204 130]) decomposedNormalization asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[97 204 130]) composedNormalization asUtf8Bytes
	"

	"Ascii is already normalized to NFC."
	| byteIndex noDiacriticalFound stopByteIndex |
	self isAscii ifTrue: [ ^self ].

	"Do a search for combining code points. If none is found, answer self."
	noDiacriticalFound _ true.
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	[ noDiacriticalFound and: [byteIndex <= stopByteIndex] ] whileTrue: [ | n |
		n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
			(UnicodeCodePoint isDiacriticalMark: codePoint)
				ifTrue: [noDiacriticalFound _ false]].
		byteIndex _ byteIndex + n ].
	noDiacriticalFound
		ifTrue: [ ^self ].

	^self cannonicalDecomposition cannonicallyOrdered cannonicalComposition! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/5/2022 12:06:22'!
decomposedNormalization
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(UnicodeCodePoint codePoint: 16r1EBF) asString
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString cannonicalDecomposition asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf32Words asArray collect: [ :e | e hex ]
	(UnicodeCodePoint codePoint: 16r1EBF) asString cannonicalDecomposition asUtf32Words asArray collect: [ :e | e hex ]

	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]).
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) cannonicalDecomposition asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) cannonicalDecomposition asUtf32Words asArray collect: [ :e | e hex ]

	(UnicodeCodePoint codePoint: 119137) asString cannonicalDecomposition asUtf32Words asArray
	(UnicodeCodePoint codePoint: 119135) asString cannonicalDecomposition asUtf32Words asArray
	"
	^self cannonicalDecomposition cannonicallyOrdered! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 12/5/2022 11:47:23'!
codePoints: arrayOfNumbers
	^self fromUtf32Words: arrayOfNumbers asWordArray! !


!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 12/5/2022 12:37:59'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory cannonicalCombiningClass characterDecompositionMapping decomposition composition |
	UppercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	CannonicalCombiningClasses _ Dictionary new.
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
			cannonicalCombiningClass _ (fields at: 4) asNumber.
			cannonicalCombiningClass = 0 ifFalse: [
				CannonicalCombiningClasses at: codePoint put: cannonicalCombiningClass ].
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


!Utf8String methodsFor: 'converting' stamp: 'jmv 12/5/2022 11:49:19'!
reversed
	"
	'frog' asUtf8String reversed
	'tomá agüita, ñandú' asUtf8String reversed
	"
	^Utf8String fromUtf32Words: self asUtf32Words reversed! !

!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: 'CannonicalCombiningClasses CompositionMapping DecompositionMapping DiacriticalMarksTable LowercaseLettersTable NamedCharactersMap SymbolsLikeTable UppercaseLettersTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!
UnicodeCodePoint initialize!
