'From Cuis 6.0 [latest update: #5572] on 6 December 2022 at 5:37:33 pm'!

!String methodsFor: 'converting' stamp: 'jmv 12/6/2022 17:30:57'!
asCodePoints
	"Convert the given string to UTF-32 from the internal encoding: ISO Latin 9 (ISO 8859-15).
	Answer a WordArray."

	| inStream nextChar codePoint |
	^WordArray streamContents: [ :outStream |
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [
			nextChar _ inStream next.
			codePoint _ nextChar codePoint.
			outStream nextPut: codePoint ]]! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 12/6/2022 17:36:46'!
asCodePoints
	"Answer a WordArray."

	^Utf8String codePointsFromUtf8: bytes! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 12/6/2022 17:29:00'!
fromCodePoints: aWordArray

	^self fromUtf8Bytes: (Utf8String utf8FromCodePoints: aWordArray) codePointCount: aWordArray size.! !

!Utf8String class methodsFor: 'services' stamp: 'jmv 12/6/2022 17:36:43'!
codePointsFromUtf8: aByteArray
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

!Utf8String class methodsFor: 'services' stamp: 'jmv 12/6/2022 17:28:26'!
utf8FromCodePoints: aWordArray
	"Convert the given string from UTF-32 to UTF-8"

	^ByteArray streamContents: [ :strm |
		aWordArray do: [ :codePoint |
			Character
				evaluate: [ :byte | strm nextPut: byte ]
				withUtf8BytesOfUnicodeCodePoint: codePoint ]]! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 12/6/2022 17:31:08'!
reversed
	"
	'frog' asUtf8String reversed
	'tomá agüita, ñandú' asUtf8String reversed
	"
	^Utf8String fromCodePoints: self asCodePoints reversed! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 17:33:16'!
basicCannonicalComposition
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #cannonicalComposition.
	(Utf8String fromCodePoints: #(119128 119141)) basicCannonicalComposition asCodePoints
	(Utf8String fromCodePoints: #(119135 119151)) basicCannonicalComposition asCodePoints
	(Utf8String fromCodePoints: #(119128 119141 119151)) basicCannonicalComposition asCodePoints
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

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 17:34:51'!
basicCannonicalDecomposition
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #cannonicalDecomposition.
	(UnicodeCodePoint codePoint: 119135) asString basicCannonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119137) asString basicCannonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex n |
	byteIndex _ 1.
	stopByteIndex _ bytes size.
	^Utf8String streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				UnicodeCodePoint cannonicalDecomposition: codePoint on: strm ].
			byteIndex _ byteIndex + n ]].! !

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 17:33:58'!
basicCannonicallyOrdered
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(Utf8String fromCodePoints: #(16r61  16r0300 16r0315 16r031B)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(Utf8String fromCodePoints: #(16r61  16r0300 16r031B 16r0315)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(Utf8String fromCodePoints: #(16r61  16r0315 16r031B 16r0300)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(Utf8String fromCodePoints: #(16r61  16r0315 16r0300 16r031B)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(Utf8String fromCodePoints: #(16r61  16r031B 16r0300 16r0315)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(Utf8String fromCodePoints: #(16r61  16r031B 16r0315 16r0300)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
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

!Utf8String methodsFor: 'Unicode normalization' stamp: 'jmv 12/6/2022 17:35:55'!
cannonicalDecomposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(UnicodeCodePoint codePoint: 16r1EBF) asString
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCannonicalDecomposition asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCannonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]).
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) basicCannonicalDecomposition asUtf8Bytes.
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) asCodePoints asArray collect: [ :e | e hex ]
	(Utf8String fromUtf8Bytes: #[195 160 204 155 204 149]) basicCannonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeCodePoint codePoint: 119137) asString basicCannonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119135) asString basicCannonicalDecomposition asCodePoints
	"
	^self basicCannonicalDecomposition basicCannonicallyOrdered! !

!methodRemoval: Utf8String class #utf32FromUtf8: stamp: 'jmv 12/6/2022 17:36:50'!
Utf8String class removeSelector: #utf32FromUtf8:!
!methodRemoval: Utf8String class #utf8FromUtf32: stamp: 'jmv 12/6/2022 17:30:06'!
Utf8String class removeSelector: #utf8FromUtf32:!
!methodRemoval: Utf8String class #codePoints: stamp: 'jmv 12/6/2022 17:29:54'!
Utf8String class removeSelector: #codePoints:!
!methodRemoval: Utf8String class #fromUtf32Words: stamp: 'jmv 12/6/2022 17:30:02'!
Utf8String class removeSelector: #fromUtf32Words:!
!methodRemoval: Utf8String #asUtf32Words stamp: 'jmv 12/6/2022 17:31:24'!
Utf8String removeSelector: #asUtf32Words!
!methodRemoval: String #asUtf32Words stamp: 'jmv 12/6/2022 17:31:25'!
String removeSelector: #asUtf32Words!
