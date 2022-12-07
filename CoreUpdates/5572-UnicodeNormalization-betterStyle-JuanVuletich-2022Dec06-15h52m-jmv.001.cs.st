'From Cuis 6.0 [latest update: #5571] on 6 December 2022 at 3:53:22 pm'!

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 15:04:09'!
cannonicalComposition
	^self! !

!CharacterSequence methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 15:04:04'!
cannonicalDecomposition
	^self! !


!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 14:58:49'!
basicCannonicalComposition
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #cannonicalComposition.
	(Utf8String codePoints: #(119128 119141)) basicCannonicalComposition asUtf32Words asArray
	(Utf8String codePoints: #(119135 119151)) basicCannonicalComposition asUtf32Words asArray
	(Utf8String codePoints: #(119128 119141 119151)) basicCannonicalComposition asUtf32Words asArray
	"
	| byteIndex stopByteIndex n combining cls |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	combining _ OrderedCollection new.
	^Utf8String streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls _ UnicodeCodePoint combiningClass: codePoint.
				cls = 0 ifTrue: [
					self basicCompose: combining on: strm.
					combining _ OrderedCollection new ].
				combining add: codePoint ].
			byteIndex _ byteIndex + n ].
		self basicCompose: combining on: strm ].! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 14:50:01'!
basicCannonicalDecomposition
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #cannonicalDecomposition.
	(UnicodeCodePoint codePoint: 119135) asString basicCannonicalDecomposition asUtf32Words asArray
	(UnicodeCodePoint codePoint: 119137) asString basicCannonicalDecomposition asUtf32Words asArray
	"
	| byteIndex stopByteIndex n |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	^Utf8String streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				UnicodeCodePoint cannonicalDecomposition: codePoint on: strm ].
			byteIndex _ byteIndex + n ]].! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 14:50:04'!
basicCannonicallyOrdered
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(Utf8String codePoints: #(16r61  16r0300 16r0315 16r031B)) basicCannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r0300 16r031B 16r0315)) basicCannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r0315 16r031B 16r0300)) basicCannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r0315 16r0300 16r031B)) basicCannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r031B 16r0300 16r0315)) basicCannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String codePoints: #(16r61  16r031B 16r0315 16r0300)) basicCannonicallyOrdered asUtf32Words asArray collect: [ :e | e hex ]
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

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 14:56:32'!
basicCompose: combiningCodePoints on: aStream
	| continue composition |
	combiningCodePoints notEmpty ifTrue: [
		continue _ combiningCodePoints size > 1.
		[continue] whileTrue: [
			continue _ false.
			composition _ UnicodeCodePoint cannonicalCompositionOf: combiningCodePoints first and: combiningCodePoints second.
			composition ifNotNil: [
				combiningCodePoints removeFirst.
				combiningCodePoints at: 1 put: composition.
				continue _ combiningCodePoints size > 1 ]].
		combiningCodePoints do: [ :e | aStream nextPutCodePoint: e ]].! !


!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/6/2022 14:40:58'!
cannonicalCompositionOf: numericCodePoint and: combiningCodePoint

	| dict |
	dict _ CompositionMapping at: numericCodePoint
		ifAbsent: [^nil ].
	^dict at: combiningCodePoint ifAbsent: nil! !

!UnicodeCodePoint class methodsFor: 'services' stamp: 'jmv 12/6/2022 14:41:04'!
cannonicalDecomposition: numericCodePoint on: aStream

	| decomposition |
	decomposition _ DecompositionMapping at: numericCodePoint
		ifAbsent: [^aStream nextPutCodePoint: numericCodePoint].
	decomposition do: [ :each |
		self cannonicalDecomposition: each on: aStream ]! !


!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 14:50:10'!
cannonicalComposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	Utf8String fromUtf8Bytes: #[195 162].
	(Utf8String fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[195 162]) cannonicalDecomposition asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[195 162]) cannonicalComposition asUtf8Bytes

	Utf8String fromUtf8Bytes: #[97 204 130].
	(Utf8String fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[97 204 130]) cannonicalDecomposition asUtf8Bytes
	(Utf8String fromUtf8Bytes: #[97 204 130]) cannonicalComposition asUtf8Bytes
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

	^self basicCannonicalDecomposition basicCannonicallyOrdered basicCannonicalComposition! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 14:47:34'!
cannonicalDecomposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(UnicodeCodePoint codePoint: 16r1EBF) asString
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCannonicalDecomposition asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf32Words asArray collect: [ :e | e hex ]
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCannonicalDecomposition asUtf32Words asArray collect: [ :e | e hex ]

	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]).
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) basicCannonicalDecomposition asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf32Words asArray collect: [ :e | e hex ]
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) basicCannonicalDecomposition asUtf32Words asArray collect: [ :e | e hex ]

	(UnicodeCodePoint codePoint: 119137) asString basicCannonicalDecomposition asUtf32Words asArray
	(UnicodeCodePoint codePoint: 119135) asString basicCannonicalDecomposition asUtf32Words asArray
	"
	^self basicCannonicalDecomposition basicCannonicallyOrdered! !

!methodRemoval: Utf8String #decomposedNormalization stamp: 'jmv 12/6/2022 15:52:33'!
Utf8String removeSelector: #decomposedNormalization!
!methodRemoval: Utf8String #cannonicallyOrdered stamp: 'jmv 12/6/2022 15:52:33'!
Utf8String removeSelector: #cannonicallyOrdered!
!methodRemoval: Utf8String #composedNormalization stamp: 'jmv 12/6/2022 15:52:33'!
Utf8String removeSelector: #composedNormalization!
