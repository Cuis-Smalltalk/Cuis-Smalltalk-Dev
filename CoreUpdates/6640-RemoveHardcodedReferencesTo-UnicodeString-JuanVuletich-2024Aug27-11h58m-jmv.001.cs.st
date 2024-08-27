'From Cuis7.1 [latest update: #6638] on 27 August 2024 at 12:00:51 pm'!

!UnicodeString methodsFor: 'copying' stamp: 'jmv 8/27/2024 11:39:37'!
appendToString: aString

	^ self species fromUtf8Bytes: aString asUtf8Bytes, bytes codePointCount: aString size + self size.! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 8/27/2024 11:42:21'!
copyFrom: startIndex to: stopIndex
	"Return a new string, a copy of me from startIndex to stopIndex, both character indices.
	Signal SubscriptOutOfBounds or CollectionIsEmpty in case of illegal indices."
	
	| startByteIndex stopByteIndex |
	startIndex = (self size+1) ifTrue: [ ^ self species empty ].
	stopIndex = 0 ifTrue: [ ^ self species empty ].
	self boundsCheck: startIndex. 
	self boundsCheck: stopIndex. 
	startByteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	stopByteIndex := stopByteIndex + (self encodedByteCountAt: stopByteIndex) - 1.
	^ self species
		fromUtf8Bytes: (bytes copyFrom: startByteIndex to: stopByteIndex)
		codePointCount: stopIndex-startIndex+1.! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 8/27/2024 11:39:47'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| epilogFirstIndex prologLastIndex newBytes replacementBytes |
	prologLastIndex := (self byteIndexAt: startIndex)-1.
	epilogFirstIndex := self byteIndexAt: endIndex+1.
	replacementBytes := replacement string asUtf8BytesOrByteString.
	newBytes := (bytes copyFrom: 1 to: prologLastIndex),
		replacementBytes,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size).
	^ self species fromUtf8Bytes: newBytes codePointCount: (startIndex-1) + replacement size + (self size-endIndex).! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 8/27/2024 11:39:54'!
copyWith: aCharacter

	^ self species
		fromUtf8Bytes: bytes, (Character utf8BytesOf: aCharacter codePoint)
		codePointCount: self size + 1.! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:22:11'!
asLowercase
	| b |
	self isAscii ifFalse: [ ^super asLowercase ].
	b := bytes copy.
	String translateToLowercase: b.
	^ self species fromUtf8Bytes: b! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:22:16'!
asUppercase
	| b |
	self isAscii ifFalse: [ ^super asUppercase ].
	b := bytes copy.
	String translateToUppercase: b.
	^ self species fromUtf8Bytes: b! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:22:45'!
substrings
	"Answer an array of the substrings that compose the receiver.
	Whitespace characters are considered separators, and not included in the result.

	'  Hello World!! Some  		1234 more  €s ' substrings

	'  Hello World!!     Some  		1234 more  €s 
	for you
	to play with ' substrings
	"
	^ self isAscii
		ifTrue: [ (String substringsIn: bytes) collect: [ :eachBytes |
				self species fromUtf8Bytes: eachBytes codePointCount: eachBytes size ]]
		ifFalse: [ (String substringsIn: bytes) collect: [ :eachBytes |
				self species fromUtf8Bytes: eachBytes ]].! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:41:05'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	| correctedBytes |
	correctedBytes := String string: bytes withLineEndings: lineEndingString asByteArray.
	"The codePointCount trick can be done because the line endings considered are all single byte UTF-8.
	If we ever consider unicode line endings taking more than one byte, this is no longer valid (pass nil instead)."
	^ self species fromUtf8Bytes: correctedBytes codePointCount: self size - bytes size + correctedBytes size.! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 8/27/2024 11:25:26'!
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
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls := Character combiningClass: codePoint.
				cls = 0 ifTrue: [
					self basicCompose: combining on: strm.
					combining := OrderedCollection new ].
				combining add: codePoint ].
			byteIndex := byteIndex + n ].
		self basicCompose: combining on: strm ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 8/27/2024 11:26:01'!
basicCanonicalDecomposition
	"
	Usually prefer #canonicalComposition or #canonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #canonicalDecomposition.
	(Character codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	(Character codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	"
	| byteIndex stopByteIndex n |
	byteIndex := 1.
	stopByteIndex := bytes size.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				Character canonicalDecomposition: codePoint on: strm ].
			byteIndex := byteIndex + n ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 8/27/2024 11:26:06'!
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
	byteIndex := 1.
	stopByteIndex := bytes size.
	combining := OrderedCollection new.
	^self species streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n := Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				cls := Character combiningClass: codePoint.
				cls = 0
					ifTrue: [
						combining notEmpty ifTrue: [
							combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
							combining do: [ :e | strm nextPutCodePoint: e third ]].
						strm nextPutCodePoint: codePoint.
						combining := OrderedCollection new ]
					ifFalse: [
						element := { cls. combining size+1. codePoint }.
						combining add: element ]].
			byteIndex := byteIndex + n ].
		combining notEmpty ifTrue: [
			combining sort: [ :a :b | a first < b first or: [ a first = b first and: [a second < b second]] ].
			combining do: [ :e | strm nextPutCodePoint: e third ] ]].! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 8/27/2024 11:40:22'!
fromBytesStream: aByteStream
	"Handle UTF-8 bytes, but convert invalid UTF-8 assuming it means ISO-8859-1.
	Argument can be, for example, a InflateStream."

	| utf8 codePointCount buffer readBytes leftover byteIndex n |
	buffer := ByteArray new: 10000.
	leftover := nil.
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[aByteStream atEnd] whileFalse: [ 
			readBytes := aByteStream nextInto: buffer.
			leftover ifNotNil: [
				readBytes := leftover, readBytes.
				leftover := nil ].
			byteIndex := 1.
			[ byteIndex <= (aByteStream atEnd
						ifFalse: [readBytes size-3] "To be sure we don't break a possible partially read multibyte code point."
						ifTrue: [readBytes size])   						"But do taks _all_ the bytes if they are the last ones."
			] whileTrue: [
				n := Character utf8BytesAndCodePointAt: byteIndex in: readBytes
					into: [ :byte1 :byte2 :byte3 :byte4 |
						utf8Stream nextPut: byte1.
						byte2 ifNotNil: [
							utf8Stream nextPut: byte2.
							byte3 ifNotNil: [
								utf8Stream nextPut: byte3.
								byte4 ifNotNil: [
									utf8Stream nextPut: byte4 ]]].
						codePointCount := codePointCount + 1]
					into: nil.
				byteIndex := byteIndex + n ].
			leftover := byteIndex > readBytes size ifFalse: [
				readBytes copyFrom: byteIndex to: readBytes size ].
		].
	].
	^self fromUtf8Bytes: utf8 codePointCount: codePointCount.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 8/27/2024 11:40:41'!
fromCodePoints: aWordArray

	^self fromUtf8Bytes: (self utf8FromCodePoints: aWordArray) codePointCount: aWordArray size.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 8/27/2024 11:27:12'!
newFrom: aSimilarObject
	^self streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: UnicodeString
"! !


!UnicodeSymbol methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:41:24'!
asPlainString
	"Answer a plain String, not a Symbol."

	^ self species fromUtf8Bytes: bytes codePointCount: self size.! !

!UnicodeSymbol methodsFor: 'converting' stamp: 'jmv 8/27/2024 11:26:13'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs offs |
	(numArgs := self numArgs) >= n ifTrue: [ ^self ].	
	^ (self species streamContents: [ :strm |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs := 0 ] ifFalse: [offs := 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol.! !

