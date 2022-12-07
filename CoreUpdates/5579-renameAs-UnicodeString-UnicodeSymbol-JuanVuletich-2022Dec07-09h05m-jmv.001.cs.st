'From Cuis 6.0 [latest update: #5578] on 7 December 2022 at 9:06:44 am'!
Smalltalk renameClassNamed: #Utf8String as: #UnicodeString!
Smalltalk renameClassNamed: #Utf8Symbol as: #UnicodeSymbol!

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
asString
	^UnicodeString with: self! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
asText
	^ (UnicodeString with: self) asText! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
asUtf8String
	"Answer an instance of the UTF-8 encoded String class"

	| utf8Bytes |
	utf8Bytes _ self asUtf8Bytes.
	^UnicodeString fromUtf8Bytes: utf8Bytes codePointCount: self size.! !


!String methodsFor: 'enumerating' stamp: 'jmv 12/7/2022 09:05:55'!
collect: aBlock
	"Refer to the comment in Collection >> #collect:."
	| array resultClass |
	array _ self collectInArray: aBlock.
	resultClass _ String.
	array do: [ :element |
		element isCharacter ifFalse: [ ^array ].
		(element is: #Character) ifFalse: [
			resultClass _ UnicodeString ]].
	^resultClass newFrom: array! !


!UnicodeString methodsFor: 'copying' stamp: 'jmv 12/7/2022 09:05:56'!
appendToString: aString

	^UnicodeString fromUtf8Bytes: aString asUtf8Bytes, bytes codePointCount: aString size + self size.! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 12/7/2022 09:05:55'!
copyFrom: startIndex to: stopIndex
	"Return a new string, a copy of me from startIndex to stopIndex, both character indices.
	Signal SubscriptOutOfBounds or CollectionIsEmpty in case of illegal indices."
	
	| startByteIndex stopByteIndex |
	startIndex = (self size+1) ifTrue: [ ^UnicodeString empty ].
	stopIndex = 0 ifTrue: [ ^UnicodeString empty ].
	self boundsCheck: startIndex. 
	self boundsCheck: stopIndex. 
	startByteIndex := self byteIndexAt: startIndex.
	stopByteIndex := self byteIndexAt: stopIndex.
	stopByteIndex := stopByteIndex + (self encodedByteCountAt: stopByteIndex) - 1.
	^UnicodeString
		fromUtf8Bytes: (bytes copyFrom: startByteIndex to: stopByteIndex)
		codePointCount: stopIndex-startIndex+1.! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 12/7/2022 09:05:55'!
copyReplaceFrom: startIndex to: endIndex with: replacement
	"Answer a copy of the receiver satisfying the following conditions: 
	+ stop is less than start, then this is an insertion; stop should be exactly start-1, 
	+ start = 1 means insert before the first character,
	+ start = size+1 means append after last character. 
	+ Otherwise, this is a replacement; start and stop have to be within the receiver's bounds."

	| epilogFirstIndex prologLastIndex newBytes replacementBytes |
	prologLastIndex _ (self byteIndexAt: startIndex)-1.
	epilogFirstIndex _ self byteIndexAt: endIndex+1.
	replacementBytes _ replacement asUtf8BytesOrByteString.
	newBytes _ (bytes copyFrom: 1 to: prologLastIndex),
		replacementBytes,
		(bytes
			copyFrom: epilogFirstIndex
			to: bytes size).
	^UnicodeString fromUtf8Bytes: newBytes codePointCount: (startIndex-1) + replacement size + (self size-endIndex).! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 12/7/2022 09:05:56'!
copyWith: anUnicodeCodePoint

	^UnicodeString
		fromUtf8Bytes: bytes, (Character utf8BytesOfUnicodeCodePoint: anUnicodeCodePoint codePoint)
		codePointCount: self size + 1.! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:05:56'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 1].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 1! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:05:55'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) <= 2].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) <= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) <= 2! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:05:56'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 3].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 3]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 3! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:05:56'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) >= 2].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) >= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) >= 2! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
asCodePoints
	"Answer a WordArray."

	^UnicodeString codePointsFromUtf8: bytes! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
reversed
	"
	'frog' asUtf8String reversed
	'tomá agüita, ñandú' asUtf8String reversed
	"
	^UnicodeString fromCodePoints: self asCodePoints reversed! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
substrings
	"
	'  Hello World!! Some  		1234 more  ' substrings
	'  Hello World!! Some  		1234 more  ' asUtf8String substrings
	"
	^ self isAscii
		ifTrue: [ (String substringsIn: bytes) collect: [ :eachBytes |
				UnicodeString fromUtf8Bytes: eachBytes codePointCount: eachBytes size ]]
		ifFalse: [ (String substringsIn: bytes) collect: [ :eachBytes |
				UnicodeString fromUtf8Bytes: eachBytes ]].! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	| correctedBytes |
	correctedBytes _ String string: bytes withLineEndings: lineEndingString asByteArray.
	"The codePointCount trick can be done because the line endings considered are all single byte UTF-8.
	If we ever consider unicode line endings taking more than one byte, this is no longer valid (pass nil instead)."
	^  UnicodeString fromUtf8Bytes: correctedBytes codePointCount: self size - bytes size + correctedBytes size.! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 09:05:56'!
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

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 09:05:56'!
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
	^UnicodeString streamContents: [ :strm |
		[ byteIndex <= stopByteIndex ] whileTrue: [
			n _ Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
				UnicodeCodePoint cannonicalDecomposition: codePoint on: strm ].
			byteIndex _ byteIndex + n ]].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 09:05:56'!
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


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:05:55'!
fromCodePoints: aWordArray

	^self fromUtf8Bytes: (UnicodeString utf8FromCodePoints: aWordArray) codePointCount: aWordArray size.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:05:56'!
newFrom: aSimilarObject
	^UnicodeString streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Utf8String
"! !

!UnicodeString class methodsFor: 'system' stamp: 'jmv 12/7/2022 09:06:18'!
symbolClassToUse
	"Answer the class to create symbols from us"
	^UnicodeSymbol! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:55'!
demoAncientEgyptian
	"
	Utf8String demoAncientEgyptian.
	"
	| utf8 fontFamily |
	utf8 _  #[240 147 128 128 240 147 133 184 240 147 137 128 240 147 144 174 240 147 128 144 240 147 128 145 240 147 128 146 32 240 147 131 176 240 147 131 177 240 147 131 178 240 147 131 179 240 147 131 180 240 147 131 181 240 147 131 182 32 240 147 130 128 32 240 147 133 184 240 147 133 185 240 147 133 186 240 147 133 187 32 240 147 133 189 32 240 147 133 190 240 147 133 191 32 240 147 137 160 240 147 137 161 240 147 137 162 240 147 137 163 240 147 137 164 32 240 147 134 178 240 147 129 183 240 147 143 164 240 147 142 159 240 147 128 128 240 147 129 144 240 147 143 165 240 147 131 128 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 133 177 240 147 128 148 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 135 139 240 147 135 139 240 147 143 143 240 147 129 144 240 147 144 141 240 147 130 139 240 147 139 180 240 147 130 157 240 147 142 155 240 147 139 169 240 147 137 148 240 147 138 170 240 147 143 155 240 147 139 180 240 147 144 160 240 147 132 191 240 147 130 139 240 147 143 143 240 147 140 151 240 147 128 129 240 147 140 183 240 147 130 157 240 147 143 143 240 147 143 173 240 147 143 155 240 147 135 190 240 147 143 143 240 147 133 147 240 147 133 177 240 147 128 128 240 147 129 144 240 147 143 170 240 147 131 128 240 147 140 162 240 147 140 162 240 147 136 150 240 147 136 150 240 147 143 155].

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'NotoEgyptianHieroglyphs'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 60))
		editLabel: 'Ancient Egyptian Hieroglyphs!!'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:56'!
demoArabic
	"
	Utf8String demoArabic
	"
	| utf8 |
	utf8 _  #[217 131 217 132 32 216 163 217 136 216 179 216 185 32 216 167 217 132 216 170 217 134 216 167 216 178 217 132 217 138 32 217 129 216 181 217 132 44 32 217 133 216 185 32 216 185 217 133 217 132 217 138 216 169 32 216 167 217 132 216 180 216 177 217 130 217 138 32 216 167 217 134 216 170 216 168 216 167 217 135 32 217 138 217 131 217 134 44 32 216 185 217 132 32 216 168 216 173 217 130 32 216 171 217 133 217 145 216 169 32 217 136 216 173 217 132 217 129 216 167 216 164 217 135 216 167 46 32 217 133 216 185 32 216 182 216 177 216 168 32 216 175 217 129 217 145 216 169 32 216 180 216 177 216 179 216 169 32 216 167 217 132 216 181 217 129 216 173 216 167 216 170 44 32 217 133 216 179 216 164 217 136 217 132 217 138 216 169 32 216 167 217 132 216 177 216 168 217 138 216 185 216 140 32 217 132 216 168 217 136 217 132 217 134 216 175 216 167 216 140 32 217 136 216 170 217 133 32 217 130 216 175 46 32 216 174 216 183 217 145 216 169 32 217 132 217 133 216 173 216 167 217 131 217 133 32 217 136 216 181 217 132 32 217 129 217 138 44 32 217 136 217 129 217 134 217 132 217 134 216 175 216 167 32 216 167 217 132 216 165 217 134 216 178 216 167 217 132 32 216 167 217 132 217 133 216 170 216 167 216 174 217 133 216 169 32 216 185 217 134 32 216 173 217 138 216 171 46 32 216 167 217 132 217 137 32 217 135 216 167 217 133 216 180 32 216 167 217 132 217 138 216 167 216 168 216 167 217 134 32 217 132 217 133 46 32 216 170 216 183 217 136 217 138 216 177 32 217 129 217 135 216 177 216 179 216 170 32 217 132 217 133 32 217 129 216 185 217 132 46 32 216 175 217 134 217 136 32 216 167 217 132 216 185 216 179 217 131 216 177 217 138 32 217 136 217 133 216 183 216 167 217 132 216 168 216 169 32 216 168 217 132 44 32 217 133 217 132 217 138 217 136 217 134 32 217 136 216 168 216 167 216 179 216 170 216 171 217 134 216 167 216 161 32 217 133 216 185 32 216 175 217 136 217 134 44 32 217 133 217 134 32 217 133 217 131 217 134 32 216 165 216 168 217 145 216 167 217 134 32 216 167 217 132 216 172 217 134 217 136 216 168 217 138 46 32 216 168 217 128 32 217 133 216 167 216 176 216 167 32 216 163 217 136 216 177 216 167 217 130 217 135 217 133 32 216 173 217 138 216 171 44 32 217 131 216 167 217 134 32 216 170 217 133 32 216 179 216 167 216 168 217 130 32 217 133 217 131 217 145 217 134 32 216 167 217 132 216 179 217 138 216 161 44 32 216 163 216 179 216 177 32 217 129 217 135 216 177 216 179 216 170 32 216 167 217 132 216 179 216 167 216 175 216 179 32 216 167 217 132 216 165 216 173 216 170 217 129 216 167 216 184 32 217 136 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24)) editLabel: 'Arabic'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:56'!
demoArmenian
	"
	Utf8String demoArmenian
	"
	| utf8 |
	utf8 _  #[213 172 213 184 213 188 213 165 213 180 32 213 171 213 186 213 189 213 184 214 130 213 180 32 213 164 213 184 213 172 213 184 213 188 32 213 189 213 171 213 169 32 213 161 213 180 213 165 213 169 44 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 165 213 184 214 130 213 171 213 189 213 180 213 184 213 164 32 213 188 213 165 213 186 213 184 214 130 213 164 213 171 213 161 213 188 213 165 32 213 184 214 130 213 169 32 213 189 213 171 213 169 46 32 213 190 213 165 213 188 213 184 32 213 188 213 165 214 134 213 184 213 188 213 180 213 171 213 164 213 161 213 182 213 189 32 213 174 213 184 214 130 32 213 189 213 171 213 169 44 32 213 164 213 165 213 162 213 165 213 169 32 213 186 213 165 213 188 214 134 213 165 213 174 213 169 213 184 32 213 189 213 161 213 172 213 184 214 130 213 169 213 161 213 169 213 184 214 130 213 189 32 213 165 213 169 32 213 174 213 184 214 130 213 180 44 32 213 176 213 161 213 189 32 213 169 213 188 213 171 213 169 213 161 213 182 213 171 32 213 182 213 184 213 182 213 184 214 130 213 180 213 165 213 189 32 213 182 213 165 46 32 213 180 213 165 213 171 32 213 182 213 184 32 213 189 213 184 213 172 213 184 214 130 213 169 213 161 32 213 172 213 161 213 162 213 184 213 188 213 165 213 189 32 213 184 213 180 213 171 213 169 213 169 213 161 213 180 44 32 213 165 213 173 32 213 163 213 188 213 161 213 165 213 174 213 184 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 189 32 213 176 213 161 213 189 44 32 213 165 213 161 32 213 180 213 165 213 172 32 214 132 213 184 214 130 213 171 213 189 32 213 182 213 171 213 189 213 172 32 213 161 213 189 213 189 213 184 214 130 213 180 46 32 213 171 213 182 32 213 161 213 180 213 165 213 169 32 213 182 213 165 213 180 213 184 213 188 213 165 32 213 184 214 130 213 189 213 184 214 130 46 32 213 165 213 188 213 161 213 169 32 213 171 213 184 214 130 213 190 213 161 213 188 213 165 213 169 32 213 165 213 161 32 213 189 213 165 213 164 44 32 213 176 213 161 213 189 32 213 161 213 172 213 171 213 161 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 174 213 184 213 188 213 186 213 184 213 188 213 161 32 213 161 213 164 46 32 213 165 213 184 213 189 32 214 134 213 161 213 189 213 169 213 171 213 164 213 171 213 171 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 180 32 213 182 213 165 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24)) 
		editLabel: 'Armenian'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:56'!
demoChinese
	"
	Utf8String demoChinese
	"
	| utf8 fontFamily |
	utf8 _  #[233 159 179 229 138 155 233 164 168 229 136 134 233 167 191 229 155 163 230 152 147 229 142 154 229 165 189 230 149 151 231 132 188 233 150 139 229 186 183 229 134 134 227 128 130 230 149 133 229 176 143 229 162 151 233 157 162 230 150 135 230 169 139 232 187 162 229 149 143 233 150 128 228 188 154 231 142 132 233 150 139 229 142 187 230 137 128 230 173 163 230 131 179 232 169 177 230 157 165 227 128 130 229 185 180 228 186 161 229 166 187 229 173 152 232 186 171 230 143 144 233 128 159 228 186 172 229 186 171 230 153 130 228 184 188 230 177 186 232 131 189 227 128 130 229 143 184 229 174 159 229 134 133 229 177 164 232 179 170 229 143 163 232 178 162 230 177 130 229 133 172 233 157 153 230 179 129 229 136 182 229 188 149 230 153 130 229 157 135 233 171 152 230 150 176 227 128 130 231 162 186 230 156 159 230 149 176 233 155 134 230 156 168 232 172 155 230 173 162 230 178 191 229 130 153 233 129 142 229 155 189 232 179 188 230 168 170 229 150 156 229 136 183 229 186 183 230 136 144 229 175 186 227 128 130 232 168 152 229 136 134 229 143 130 229 145 188 229 179 182 229 155 189 232 170 160 230 168 169 229 155 158 230 173 163 229 185 180 232 187 141 230 167 152 228 187 138 229 132 170 230 150 135 231 166 129 229 144 166 227 128 130 230 149 180 230 130 163 232 182 163 231 164 190 230 152 142 230 164 156 231 149 170 230 158 156 230 142 168 231 178 149 228 184 150 230 156 172 229 176 143 231 132 161 231 167 176 231 148 186 227 128 130 230 138 149 230 160 188 229 140 187 229 190 140 228 187 187 231 179 187 232 187 162 230 148 191 230 157 161 232 169 177 230 142 178 230 156 136 231 144 131 227 128 130 230 130 148 233 133 184 232 166 139 229 164 169 229 134 153 229 164 137 229 143 130 233 153 184 229 155 155 229 144 141 229 140 150 232 139 151 229 141 136 230 178 187 233 154 155 228 189 143 232 166 135 228 184 184 230 149 153 231 149 140].

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'KiwiMaru'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 24))
		editLabel: 'Chinese'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:55'!
demoCyrillic
	"
	Utf8String demoCyrillic
	"
	| utf8 |
	utf8 _  #[208 155 208 190 209 128 208 181 208 188 32 208 184 208 191 209 129 209 131 208 188 32 208 180 208 190 208 187 208 190 209 128 32 209 129 208 184 209 130 32 208 176 208 188 208 181 209 130 44 32 208 180 208 181 208 177 208 181 209 130 32 209 129 208 176 208 187 209 131 209 130 208 176 208 189 208 180 208 184 32 208 188 208 181 208 184 32 208 181 208 184 46 32 208 144 208 189 32 209 133 208 184 209 129 32 208 179 209 128 208 176 208 181 209 134 208 181 32 209 131 209 130 208 176 208 188 209 131 209 128 32 208 180 208 190 208 187 208 190 209 128 209 131 208 188 44 32 208 187 208 190 209 128 208 181 208 188 32 208 191 209 128 208 184 208 189 209 134 208 184 208 191 208 181 209 129 32 208 188 208 190 208 187 208 181 209 129 209 130 208 184 208 176 208 181 32 208 180 209 131 208 190 32 208 184 208 189 46 32 208 149 208 184 32 208 188 208 190 208 178 208 181 209 130 32 208 189 208 190 209 129 209 130 209 128 208 190 32 208 190 208 191 209 130 208 184 208 190 208 189 32 208 189 208 176 208 188 44 32 208 181 208 190 209 129 32 208 181 208 191 208 184 209 134 209 131 209 128 208 184 32 208 190 208 188 208 184 209 130 209 130 208 176 208 189 209 130 209 131 209 128 32 209 131 209 130 46 32 208 163 209 130 32 208 188 209 131 208 189 208 181 209 128 208 181 32 208 187 208 176 208 177 208 190 209 128 208 181 32 208 184 208 189 209 130 208 181 208 187 208 187 208 181 208 179 208 181 208 177 208 176 209 130 32 209 131 209 129 209 131 44 32 208 181 208 176 32 209 129 208 181 208 180 32 208 180 208 181 208 177 208 181 209 130 32 208 187 209 131 208 191 209 130 208 176 209 130 209 131 208 188 44 32 208 178 208 184 208 180 208 184 209 130 32 208 184 208 187 208 187 209 131 208 180 32 208 184 209 131 208 180 208 184 209 134 208 190 32 209 143 209 131 208 190 32 209 131 209 130 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Cyrillic'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:55'!
demoGeorgian
	"
	Utf8String demoGeorgian
	"
	| utf8 |
	utf8 _  #[225 131 154 225 131 157 225 131 160 225 131 148 225 131 155 32 225 131 152 225 131 164 225 131 161 225 131 163 225 131 155 32 225 131 147 225 131 157 225 131 154 225 131 157 225 131 160 32 225 131 161 225 131 152 225 131 151 32 225 131 144 225 131 155 225 131 148 225 131 151 44 32 225 131 167 225 131 163 225 131 157 32 225 131 163 225 131 151 32 225 131 147 225 131 152 225 131 170 225 131 144 225 131 151 32 225 131 144 225 131 154 225 131 152 225 131 167 225 131 163 225 131 152 225 131 147 44 32 225 131 144 225 131 147 32 225 131 154 225 131 152 225 131 145 225 131 160 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 160 225 131 148 225 131 164 225 131 160 225 131 148 225 131 176 225 131 148 225 131 156 225 131 147 225 131 163 225 131 156 225 131 151 32 225 131 148 225 131 161 225 131 151 46 32 225 131 154 225 131 148 225 131 146 225 131 148 225 131 160 225 131 148 32 225 131 170 225 131 157 225 131 160 225 131 164 225 131 157 225 131 160 225 131 144 32 225 131 148 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 144 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 144 225 131 151 32 225 131 148 225 131 144 225 131 155 44 32 225 131 156 225 131 157 32 225 131 167 225 131 163 225 131 144 225 131 161 32 225 131 155 225 131 148 225 131 156 225 131 144 225 131 156 225 131 147 225 131 160 225 131 152 32 225 131 160 225 131 148 225 131 164 225 131 148 225 131 160 225 131 160 225 131 148 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 167 225 131 163 225 131 152 46 32 225 131 151 225 131 148 32 225 131 164 225 131 163 225 131 151 225 131 148 225 131 156 225 131 151 32 225 131 155 225 131 157 225 131 154 225 131 154 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 149 225 131 152 225 131 161 46 32 225 131 149 225 131 152 225 131 155 32 225 131 147 225 131 148 225 131 156 225 131 152 225 131 167 225 131 163 225 131 148 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 151 225 131 176 225 131 148 225 131 157 225 131 164 225 131 176 225 131 160 225 131 144 225 131 161 225 131 151 225 131 163 225 131 161 32 225 131 148 225 131 174 44 32 225 131 148 225 131 144 32 225 131 176 225 131 144 225 131 161 32 225 131 149 225 131 157 225 131 154 225 131 163 225 131 155 225 131 163 225 131 161 32 225 131 164 225 131 144 225 131 161 225 131 151 225 131 152 225 131 147 225 131 152 225 131 152 32 225 131 147 225 131 148 225 131 161 225 131 148 225 131 160 225 131 163 225 131 156 225 131 151 46 32 225 131 148 225 131 174 32 225 131 157 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 152 225 131 161 32 225 131 151 225 131 157 225 131 160 225 131 167 225 131 163 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 155 225 131 148 225 131 152 44 32 225 131 147 225 131 157 225 131 170 225 131 151 225 131 163 225 131 161 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 152 225 131 156 32 225 131 167 225 131 163 225 131 157 46 32 225 131 161 225 131 163 225 131 155 225 131 155 225 131 157 32 225 131 151 225 131 160 225 131 144 225 131 170 225 131 151 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 148 225 131 152 32 225 131 152 225 131 163 225 131 161 44 32 225 131 163 225 131 151 32 225 131 157 225 131 155 225 131 156 225 131 148 225 131 161 32 225 131 149 225 131 152 225 131 160 225 131 151 225 131 163 225 131 151 225 131 148 32 225 131 147 225 131 148 225 131 151 225 131 160 225 131 144 225 131 174 225 131 152 225 131 151 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Georgian'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:56'!
demoGreek
	"
	Utf8String demoGreek
	"
	| utf8 |
	utf8 _  #[206 155 206 191 207 129 206 181 206 188 32 206 185 207 128 207 131 206 184 206 188 32 206 180 206 191 206 187 206 191 207 129 32 207 131 206 185 207 132 32 206 177 206 188 206 181 207 132 44 32 206 188 206 191 206 180 206 191 32 118 206 181 206 189 206 185 206 177 206 188 32 118 206 185 206 180 206 181 207 129 206 181 207 129 32 206 185 206 189 32 99 206 184 206 188 44 32 206 181 206 190 32 206 177 206 184 207 132 206 181 206 188 32 206 188 206 177 206 187 206 185 207 131 32 206 191 207 134 207 134 206 181 206 189 206 180 206 185 207 132 32 206 181 207 131 207 132 46 32 206 153 206 180 32 207 132 206 191 206 187 206 187 206 185 207 132 32 207 131 206 184 207 131 99 206 185 207 128 206 185 207 132 32 207 131 206 177 206 187 206 184 207 132 206 177 207 132 206 184 207 131 32 113 206 184 206 185 44 32 99 206 184 32 113 206 184 206 177 206 189 206 180 206 191 32 207 128 207 129 206 191 207 128 207 129 206 185 206 177 206 181 32 206 185 206 188 207 128 206 181 207 129 206 180 206 185 206 181 207 132 32 118 206 181 206 187 44 32 206 188 206 181 206 177 32 206 181 207 132 32 206 181 207 129 207 129 206 181 206 188 32 206 179 206 187 206 191 207 129 206 185 206 177 207 132 206 184 207 129 46 32 206 149 206 190 206 181 207 129 99 206 185 32 206 180 206 181 207 134 206 185 206 189 206 185 207 132 206 185 206 191 206 189 206 181 206 188 32 206 188 206 181 206 187 32 206 181 206 177 46 32 206 149 206 190 32 207 131 206 181 206 177 32 206 181 206 187 206 185 207 132 32 206 180 206 185 207 131 207 131 206 181 206 189 207 132 206 185 206 181 207 132 44 32 206 188 206 177 206 187 206 185 207 131 32 207 132 207 129 206 185 207 132 206 177 206 189 206 185 32 118 206 185 207 131 32 206 185 206 180 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Greek'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:56'!
demoHebrew
	"
	Utf8String demoHebrew
	"
	| utf8 |
	utf8 _  #[215 162 215 156 32 215 158 215 144 215 158 215 168 32 215 150 215 167 215 149 215 167 32 215 169 215 147 215 168 215 149 215 170 32 215 144 215 168 215 165 44 32 215 148 215 158 215 156 215 166 215 170 32 215 158 215 147 215 149 215 153 215 167 215 153 215 157 32 215 169 215 155 215 156 32 215 169 215 156 44 32 215 162 215 153 215 166 215 149 215 145 32 215 160 215 153 215 149 215 149 215 152 32 215 169 215 153 215 170 215 149 215 164 215 153 215 170 32 215 162 215 149 215 147 32 215 145 46 32 215 161 215 147 215 168 32 215 144 215 149 32 215 168 215 164 215 149 215 144 215 148 32 215 169 215 153 215 158 215 149 215 169 215 153 32 215 158 215 149 215 162 215 158 215 147 215 153 215 157 46 32 215 148 215 153 215 144 32 215 164 215 153 215 161 215 153 215 167 215 148 32 215 148 215 146 215 168 215 164 215 153 215 157 32 215 169 215 153 215 158 215 149 215 169 215 153 215 153 215 157 32 215 169 215 156 46 32 215 169 215 155 215 156 32 215 162 215 156 32 215 162 215 150 215 168 215 148 32 215 156 215 148 215 164 215 149 215 154 32 215 144 215 149 215 149 215 153 215 168 215 149 215 160 215 144 215 149 215 152 215 153 215 167 215 148 44 32 215 144 215 160 215 144 32 215 144 215 170 32 215 162 215 150 215 168 215 148 32 215 156 215 158 215 160 215 149 215 162 32 215 156 215 158 215 151 215 153 215 167 215 148 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Hebrew'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:05:56'!
demoLatin
	"
	Utf8String demoLatin
	"
	| utf8 |
	utf8 _  #[76 111 114 101 109 32 105 112 115 117 109 32 100 111 108 111 114 32 115 105 116 32 97 109 101 116 44 32 112 111 112 117 108 111 32 108 97 116 105 110 101 32 97 108 105 113 117 105 100 32 112 114 111 32 110 111 44 32 115 101 100 32 116 101 32 103 114 97 101 99 111 32 112 101 114 99 105 112 105 116 32 97 100 105 112 105 115 99 105 110 103 46 32 68 101 102 105 110 105 101 98 97 115 32 100 105 115 115 101 110 116 105 97 115 32 109 101 97 32 105 110 44 32 118 101 108 32 110 117 108 108 97 32 116 111 108 108 105 116 32 101 120 46 32 65 108 105 113 117 105 112 32 112 101 114 115 101 113 117 101 114 105 115 32 101 108 111 113 117 101 110 116 105 97 109 32 104 105 115 32 101 116 46 32 67 117 32 100 105 97 109 32 110 111 118 117 109 32 97 110 105 109 97 108 32 118 105 120 44 32 110 97 109 32 101 116 32 112 114 111 98 97 116 117 115 32 116 114 97 99 116 97 116 111 115 32 100 101 116 101 114 114 117 105 115 115 101 116 46 32 80 101 114 99 105 112 105 116 32 101 102 102 105 99 105 97 110 116 117 114 32 112 114 105 32 101 120 44 32 105 100 32 97 110 105 109 97 108 32 100 101 98 105 116 105 115 32 118 117 108 112 117 116 97 116 101 32 101 111 115 44 32 97 100 32 105 117 115 32 112 111 115 115 101 32 101 114 114 111 114 32 112 111 112 117 108 111 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Latin Alphabet'! !


!UnicodeSymbol methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:56'!
asPlainString
	"Answer a plain String, not a Symbol."

	^UnicodeString fromUtf8Bytes: bytes codePointCount: self size.! !

!UnicodeSymbol methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:55'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ 
	
	Note: Unrelated, but remember that we inherit #numArgs " 

	| numArgs offs |
	(numArgs _ self numArgs) >= n ifTrue: [ ^self ].	
	^ (UnicodeString streamContents: [ :strm |
		strm nextPutAll: self.
		(numArgs = 0) ifTrue: [ strm nextPutAll: ':'. offs _ 0 ] ifFalse: [offs _ 1].
		2 to: n - numArgs + offs do: [ :i | strm nextPutAll: 'with:' ]]) asSymbol! !

!UnicodeSymbol methodsFor: 'private' stamp: 'jmv 12/7/2022 09:05:56'!
species
	^UnicodeString! !


!UnicodeSymbol class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:05:55'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ (UnicodeString newFrom: aCollection) asSymbol

"
	Utf8Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Utf8Symbol
"! !


!ArrayOfCharactersAndCodePoints methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:05:55'!
asStreamResult
	"See senders.
	Also see #thatCanHoldCodePoints"
	^UnicodeString newFrom: self! !


!Utf8EncodedWriteStream methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:05:56'!
utf8Contents
	"Answer the Utf8String built so far."

	^UnicodeString fromUtf8Bytes: self byteContents codePointCount: numberOfCodePointsWritten.! !


!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 12/7/2022 09:05:56'!
newLine: aKeyboardEvent

	self addString: (UnicodeString with: Character newLineCharacter).
	^false! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 12/7/2022 09:05:56'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	self addString: (UnicodeString with: aKeyboardEvent keyCharacter).
	^false! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 12/7/2022 09:05:56'!
returnKey: aKeyboardEvent
	"Return / Enter / key was pressed"
	"Process the various Return / Enter keystrokes"
	
	morph acceptOnCR ifTrue: [
		^ true].
	aKeyboardEvent commandAltKeyPressed ifTrue: [
		(aKeyboardEvent controlKeyPressed | aKeyboardEvent rawMacOptionKeyPressed) ifTrue: [
			self addString: UnicodeString crString.
			^false ].
		self addString: UnicodeString crlfString.
		^false ].
	^ self newLine: aKeyboardEvent! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 12/7/2022 09:05:55'!
enclose: aKeyboardEvent
	"Insert or remove bracket characters around the current selection."
	"This is a user command, and generates undo"

	| left right startIndex stopIndex oldSelection which |
	startIndex _ self startIndex.
	stopIndex _ self stopIndex.
	oldSelection _ self selection.
	which _ '([<{"''`' indexOf: aKeyboardEvent keyCharacter ifAbsent: [ ^true ].
	left _ '([<{"''`' at: which.
	right _ ')]>}"''`' at: which.
	((startIndex > 1 and: [stopIndex <= model textSize])
			and: [ (model actualContents at: startIndex-1) = left and: [(model actualContents at: stopIndex) = right]])
		ifTrue: [
			"already enclosed; strip off brackets"
			self selectFrom: startIndex-1 to: stopIndex.
			self replaceSelectionWith: oldSelection]
		ifFalse: [
			"not enclosed; enclose by matching brackets"
			self replaceSelectionWith:
				(Text string: (UnicodeString with: left) attributes: emphasisHere),
				oldSelection,
				(Text string: (UnicodeString with: right) attributes: emphasisHere).
			self selectFrom: startIndex+1 to: stopIndex].
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 12/7/2022 09:05:55'!
inOutdent: aKeyboardEvent delta: delta
	"Add/remove a tab at the front of every line occupied by the selection.
	This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart size inStream outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart _ self startIndex.
	realStop _ self stopIndex - 1.

	"Special case: the text cursor on a line of its own, including weird case at end of paragraph"
	(realStart > realStop and: [
				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])
		ifTrue: [
			delta < 0
				ifTrue: [
					morph flash]
				ifFalse: [
					self replaceSelectionWith: Character tab asSymbol.
					self deselectAndPlaceCursorAt: realStart + 1].
			^true].

	lines _ textComposition lines.
	startLine _ textComposition lineIndexFor: realStart.
	stopLine _ textComposition lineIndexFor: (realStart max: realStop).
	start _ (lines at: startLine) first.
	stop _ (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart _ realStart > start.

	size _  stop + 1 - start.
	inStream _ self privateCurrentString readStreamFrom: start to: stop.
	outStream _ UnicodeString writeStream.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].
	realStop _ realStop + outStream position - size.

	"Prepare for another iteration"
	self selectInvisiblyFrom: start to: stop.
	self replaceSelectionWith: outStream contents.
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true! !

!TextEditor methodsFor: 'private' stamp: 'jmv 12/7/2022 09:05:55'!
firstOfBeginningOfLineOrEndOfIndentationLeftOf: position
	
	"Returns the first of beginning-of-line or end-of-indentation that appears to the left of the given position, wrapping around to the end of the line (i.e. the line is considered circular).
	This way, if the given position is beginning-of-line then end-of-indentation is returned."
	
	| currentLine beginningOfLine endOfIndentation stops |
	
	currentLine _ textComposition lines at: (textComposition lineIndexFor: position).
	beginningOfLine _ currentLine first.
	endOfIndentation _ self privateCurrentString
		skipDelimiters: (UnicodeString with: Character tab)
		startingAt: beginningOfLine.
		
	stops _ OrderedCollection with: endOfIndentation with: beginningOfLine.
	^ stops detect: [ :stop | stop < position ] ifNone: [endOfIndentation]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 12/7/2022 09:05:55'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^UnicodeString streamContents: [ :stream |
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: aSourceCode		]! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 12/7/2022 09:05:55'!
newLine: aKeyboardEvent
	"Replace the current text selection with a newLine (i.e. LF) followed by as many tabs
	as there are leading tabs on the current line (+/- bracket count)."

	| char s i tabCount stopIndex newLineString |
	s _ self privateCurrentString.
	stopIndex _ self stopIndex.
	i _ stopIndex.
	tabCount _ 0.
	[ (i _ i-1) > 0 and: [ (char _ s at: i) isLineSeparator not ] ] whileTrue: [
		"Count brackets"
		char = $[ ifTrue: [tabCount _ tabCount + 1].
		char = $] ifTrue: [tabCount _ tabCount - 1]].
	[ (i _ i + 1) < stopIndex and: [ (char _ s at: i) isSeparator ] ] whileTrue: [
		"Count leading tabs"
		char = Character tab ifTrue: [ tabCount _ tabCount + 1 ]].
	"Now inject newline with tabCount tabs, generating a new undoable command"
	newLineString _ UnicodeString streamContents: [ :strm | strm newLineTab: tabCount ].
	model startNewUndoRedoCommand.
	self replaceSelectionWith: newLineString.
	^ false! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 12/7/2022 09:05:55'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string _ self privateCurrentString.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	self pointIndex - 1 to: (self pointIndex - 20 max: 1) by: -1 do: [:i|
		(string at: i) = $\ ifTrue: 
			[key _ string copyFrom: i + 1 to: self pointIndex - 1.
			UnicodeCodePoint namedCharactersMap at: key ifPresent: [:value| self selectFrom: i to: self pointIndex - 1; replaceSelectionWith: (UnicodeString with: value)].
			^ super normalCharacter: aKeyboardEvent]].
	^ super normalCharacter: aKeyboardEvent! !


!FileIOAccessor methodsFor: 'primitives' stamp: 'jmv 12/7/2022 09:05:56'!
primFixedNameLookupEntryIn: fullPathAsUtf8Bytes index: index
	"Fix the malformed String answered by the primitive. See comment at #primLookupEntryIn:index:"

	| answer |
	answer _ self primLookupEntryIn: fullPathAsUtf8Bytes index: index.
	answer isArray ifTrue: [
		answer at: 1 put: (UnicodeString fromUtf8Bytes: answer first asByteArray) ].
	^answer! !


!Utf8ReadAdaptor methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:05:55'!
collectionSpecies
	^UnicodeString! !

!Utf8ReadAdaptor methodsFor: 'reading' stamp: 'jmv 12/7/2022 09:05:56'!
next: n
	"Read n UnicodeCodePoints, or until the end of the file.
	Answer them, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Answer is an instance of Utf8String."

	| utf8 codePointCount |
	codePointCount _ 0.
	utf8 _ ByteArray streamContents: [ :utf8Stream |
		[ codePointCount = n or: [stream atEnd] ] whileFalse: [
			stream
				nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
					utf8Stream nextPut: byte1.
					byte2 ifNotNil: [
						utf8Stream nextPut: byte2.
						byte3 ifNotNil: [
							utf8Stream nextPut: byte3.
							byte4 ifNotNil: [
								utf8Stream nextPut: byte4 ]]].
					codePointCount _ codePointCount + 1]
				into: nil ]].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.! !

!Utf8ReadAdaptor methodsFor: 'reading' stamp: 'jmv 12/7/2022 09:05:56'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Leave stream at the end position."

	| utf8 codePointCount |
	codePointCount _ 0.
	utf8 _ ByteArray streamContents: [ :utf8Stream |
		[ stream atEnd ] whileFalse: [
			stream
				nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
					utf8Stream nextPut: byte1.
					byte2 ifNotNil: [
						utf8Stream nextPut: byte2.
						byte3 ifNotNil: [
							utf8Stream nextPut: byte3.
							byte4 ifNotNil: [
								utf8Stream nextPut: byte4]]].
					codePointCount _ codePointCount + 1]
				into: nil ]].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:05:55'!
retrieveStringOrText
	"Answer a Text or a String. Appropriate for a Text Editor."
	| answer |

	answer _ self retrieveObject.
	
	"If clipboard contents is already a Text or String, answer it."
	(answer isString or: [ answer is: #Text ]) ifTrue: [
		^answer ].
	
	"If clipboard contains a Morph, answer a TextAnchor incuding it..."
	(answer is: #Morph) ifTrue: [
		^ (Text string: (UnicodeString with: $*) attribute: (TextAnchor new anchoredFormOrMorph: answer)) ].
	
	"If clipboard contains a Form, prepare a Text including it."
	(answer is: #Form) ifTrue: [
		^Text withForm: answer ].
	
	"Answer some string..."
	^answer ifNil: [ UnicodeString new ] ifNotNil: [ answer printString ]! !

!Clipboard methodsFor: 'private' stamp: 'jmv 12/7/2022 09:05:55'!
retrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| utf8Bytes |
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].

	utf8Bytes _ self primitiveClipboardString.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard."
	utf8Bytes isEmpty ifTrue: [ ^nil ].

	"Abuse the fact that the VM provides an instance of String (although it contains UTF-8 bytes)"
	utf8Bytes _ utf8Bytes fixLineEndings.

	"Clipboard primitive answers an instance of String containing the UTF-8 bytes.
	If not pure ASCII, contents are invalid!! Convert it ASAP into a reasonable ByteArray."
	utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
		ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
		ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
	^ UnicodeString fromUtf8Bytes: utf8Bytes.! !


!DropFilesEvent methodsFor: 'files' stamp: 'jmv 12/7/2022 09:05:56'!
fileNamesDo: aBlock

	1 to: numberOfFiles do: [ :fileNumber | | utf8Bytes |
		(self primDropRequestFileName: fileNumber) ifNotNil: [ :primUtf8Bytes |
		utf8Bytes _ primUtf8Bytes.
		utf8Bytes class format = ByteArray format  												"True unless image is in 32bit V3 format"
			ifTrue: [ ByteArray adoptInstance: utf8Bytes ]
			ifFalse: [ utf8Bytes _ ByteArray withAll: utf8Bytes ].
		aBlock value: (UnicodeString fromUtf8Bytes: utf8Bytes) ]].! !


!UnicodeCodePoint reorganize!
('accessing' asciiValue codePoint digitValue numericValue)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isCharacter isDiacriticalMark isDigit isLetter isLineSeparator isLowercase isNullCharacter isSeparator isSymbolLike isUnaccented isUnicodeCodePoint isUppercase isValidInBinarySelectors isValidInIdentifiers isValidStartOfIdentifiers isVowel iso8859s15Code tokenish)
('copying' shallowCopy)
('comparing' < <= = > >= hash)
('converting' asCharacter asLowercase asString asSymbol asText asUnaccented asUnicodeCodePoint asUppercase to: withDiacriticalMark:)
('printing' hex isLiteral printOn: storeOn:)
('private' privateValue:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!CharacterSequence reorganize!
('accessing' findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOfSubCollection:startingAt:ifAbsent: lineCount lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string substringsSeparatedBy:)
('adding' add:)
('copying' append: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('comparing' alike: beginsWith: commonPartWith:startAt:stopAt:applying: commonPrefixWith: endsWith: is:substringAt: match: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: afterBlanksEndsWith: asByteString asCamelCase asDate asFileName asIdentifier: asLowercase asMonth asNumber asPlainString asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8String asUtf8StringOrText asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withNewLines withoutEnclosing: withoutLeadingBlanks withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect: collectInArray:)
('printing' print printOn: storeOn:)
('testing' isAlphaNumeric isRemote isString)
('user interface' edit editLabel:)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('arithmetic' * + - / // \\)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom:)
('formatting' format:)
('uCompletion' separateKeywords)
('paragraph support' endOfParagraphBefore: indentationIfBlank:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('fileman-actions' fileContents fileContents:)
('Unicode normalization' cannonicalComposition cannonicalDecomposition)
!


!String reorganize!
('accessing' at: at:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString:)
('converting' asByteString asCodePoints asHex asLowercase asUppercase asUtf8Bytes asUtf8BytesOrByteString capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('private' replaceFrom:to:with:startingAt:)
('services' numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty isInCannonicalCompositionForm)
('paragraph support' encompassLine: encompassParagraph:)
!


!UnicodeString reorganize!
('accessing' at: bytes findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with: copyWith: shallowCopy)
('comparing' < <= = > >= basicHash caseInsensitiveLessOrEqual: computeHash hash is:substringAt: sameAs:)
('converting' asByteArray asByteString asCodePoints asHex asUtf8Bytes asUtf8String reversed substrings surroundedBySingleQuotes thatCanBeModified withLineEndings:)
('enumerating' do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' isAscii isEmpty isInCannonicalCompositionForm isUtf8String)
('paragraph support' encompassLine: encompassParagraph:)
('printing' isLiteral)
('private' boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: privateBytes:codePointCount: stride)
('services' numArgs)
('initialization' initialize)
('dispatch' displayOnBitBltCanvasEngine:from:to:at:font:color: displayOnVectorEngine:from:to:at:font:color:)
('Unicode normalization' basicCannonicalComposition basicCannonicalDecomposition basicCannonicallyOrdered basicCompose:on: cannonicalComposition cannonicalDecomposition findCannonicalCompositionAndHash)
!


!UnicodeString class reorganize!
('instance creation' crString crlfString empty fromCodePoints: fromUtf8Bytes: fromUtf8Bytes:codePointCount: lfString new:withAll: newFrom: newLineString tab with: with:with: with:with:with: withAll:)
('services' codePointsFromUtf8: compare:with: utf8FromCodePoints: utf8FromUtf16:)
('system' symbolClassToUse)
('stream creation' streamContents: writeStream)
('unicode demo' demoAncientEgyptian demoArabic demoArmenian demoChinese demoCyrillic demoGeorgian demoGreek demoHebrew demoLatin)
!


!UnicodeSymbol reorganize!
('accessing' precedence)
('comparing' =)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('testing' isInfix isInitializePvtSelector isKeyword isPvtSelector isSymbol isTestSelector isUnary isValidBinarySelector isValidKeywordSelector isValidSelector isValidUnarySelector testSelectorPrefix)
('printing' isLiteral print storeOn:)
('uCompletion' separateKeywords)
('private' species)
('system primitives' flushCache)
('object serialization' comeFullyUpOnReload:)
!


!UnicodeSymbol class reorganize!
('instance creation' fromCollectionOfStrings: newFrom: readFrom:)
('private' basicFromString:)
('testing' testSelectorPrefix)
!


!ArrayOfCharactersAndCodePoints reorganize!
('accessing' at:)
('converting' asStreamResult encompassLine: encompassParagraph:)
('testing' is:)
!


!Utf8EncodedWriteStream reorganize!
('read, write, position' next:putAll:startingAt: nextPut: nextPutAll: nextPutCodePoint: pastEndPutByte: peekLast position: skip skip: skipBack)
('private' byteContents isCompatibleWithContents: nextPutByte: on: pastEndPut:)
('accessing' collectionSpecies contents utf8Contents)
('positioning' reset resetToStart setToEnd)
!


!Editor reorganize!
('accessing' currentAttributes help morph morph: userHasEdited)
('accessing-selection' selectionInterval)
('editing keys' copySelection: cut: noop: paste:)
('menu messages' clipboardStringOrText clipboardTextPut: lineSelectAndEmptyCheck: paste pasteString wordSelectAndEmptyCheck:)
('new selection' deselectAndPlaceCursorAt: selectInterval: selectInvisiblyFrom:to: selectInvisiblyMark:point: selectMark:point: selectWord selectWordLeftDelimiters:rightDelimiters: selectWordOrDelimitedText wordLeftDelimiters wordRangeLeftDelimiters:rightDelimiters: wordRangeLeftDelimiters:rightDelimiters:startingAt: wordRangeUnder: wordRightDelimiters)
('nonediting/nontyping keys' cursorDown: cursorLeft: cursorPageDown: cursorPageUp: cursorRight: cursorUp: selectWord:)
('typing/selecting keys' backspace: cursorTopHome: enter: newLine: normalCharacter: returnKey: selectAll selectAll:)
('private' beginningOfLine: beginningOfNextParagraph: beginningOfParagraph: beginningOfText endOfLine: endOfParagraph: endOfText moveCursor:forward:event: nextWordEnd: nextWordEndFrom:goingForwards: nextWordStart: nextWordStartFrom:goingForwards: previousWordStart: sameColumn:newLine:forward: setIndices:forward:)
('menu commands' offerMenuFromEsc:)
('typing support' cmdShortcuts shortcuts)
!


!TextEditor reorganize!
('accessing' actualContents actualContents: currentAttributes currentCharacterStyleOrNil currentParagraphStyle defaultFont defaultFont: lastFont lastParagraphStyleOrNil pointBlock replaceSelectionWith: selectionRectangle setSearch: text totalTextHeight userHasEdited visibleHeight)
('accessing-selection' characterBlockForIndex: hasSelection markIndex markIndex: markIndex:pointIndex: pointIndex pointIndex: selectedString selection startBlock startIndex stopBlock stopIndex)
('attributes' clearFont offerColorMenu offerDefaultFontMenu offerFontMenu offerFontStyleMenu)
('current selection' recomputeSelection)
('displaying' flash)
('editing keys' align: changeEmphasis: changeLineEndsToLf: chooseColor compareToClipboard: copyHiddenInfo enclose: hiddenInfo inOutdent:delta: indent: makeCapitalized: makeLowercase: makeUppercase: offerColorMenu: offerFontMenu: outdent: tabKey: undo:)
('events' clickAndHalf doubleClickAndHalf mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('menu messages' acceptContents align chooseAlignment compareToClipboard copySelection cut find findAgain openHelp pasteRecent setSearchString wrapOnOff)
('model access' model: wordUnder: wordUnderCursor)
('new selection' afterSelectionInsertAndSelect: correctFrom:to:with: insertAndSelect:at: lineSelectAndEmptyCheck: messageSendsRanges: selectFrom:to: selectLine)
('nonediting/nontyping keys' cursorEnd: cursorHome: help: setSearchString:)
('parenblinking' blinkParen blinkParenAt: blinkPrevParen: clearParens)
('typing support' addString: backTo: dispatchOn: processKeystrokeEvent: setEmphasisHereFromText setEmphasisHereFromTextForward:)
('typing/selecting keys' backWord: find: findAgain: forwardDelete:)
('private' addAttributesForPasting: applyAttribute: beginningOfLine: endOfLine: findAndReplaceMany: findAndReplaceOnce: firstOfBeginningOfLineOrEndOfIndentationLeftOf: indent:fromStream:toStream: isDisjointFrom: moveCursor:forward:event: nullText pageHeight privateCurrentString sameColumn:newLine:forward: shouldDeleteAWordForward: shouldHandleUsingCmdShortcuts: storeSelectionInComposition unapplyAttributes:)
('binding' bindingOf:)
('commands' insertMorph:at: removeMorph:)
('undo & redo' flushUndoRedoCommands offerUndoHistory redo redo: undo)
('menu' addCutAndPasteMenuSectionTo: addFinderMenuSectionTo: addStyleMenuSectionTo: addUndoMenuSectionTo: getMenu)
('initialization' initialize resetState textComposition:)
('testing' hasUnacceptedEdits hasUnacceptedEdits:)
!


!SmalltalkEditor reorganize!
('contextual add/remove parameter' changeSelector:in:at:using: changeSelectorOf:in:at:using: changeSelectorTo:in:using: contextualAddParameter contextualAddParameter: contextualChangeSelectorInMethodUsing: contextualChangeSelectorOf:in:using: contextualChangeSelectorUsing: contextualExtractAsParameter contextualExtractAsParameter: contextualRemoveParameter contextualRemoveParameter: extractAsParameter)
('contextual push up/down inst. var' apply:inClassDefinitionOf:in: contextualPushDownInClassDefinition contextualPushUpInClassDefinition ifEditingClassDefinitionDoOrWarn: inClassDefinitionContextuallyApply: informRefactoringCanOnlyBeAppliedInClassDefinition)
('contextual rename' contextualRename contextualRename: contextualRenameInClassDefinition contextualRenameInClassDefinitionOf:in: contextualRenameInMethod contextualRenameOf:in: ifSourceCodeRefactoringCanBeAppliedDo: isEditingClassDefinition rename:in:at: renameClassOn:for: renameGlobalOn:for: renameInstanceVariableOn:for:at: renameSelectorFor:in: renameSelectorOf:in:at: renameTemporary:at: selectedClassOrMetaClassOrUndefinedObject withClassDefinitionNodeAndClassDo:ifErrorsParsing: withMethodNodeAndClassDo:ifErrorsParsing:)
('do-its' afterCompiling:do:for:in:ifFail: afterCompiling:do:ifFail: debug:receiver:in: debugIt doIt doItProfiling: doItSourceCodeFor:in: evaluate:andDo:ifFail:profiled: evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt inspectSelectionOrLine printIt profileIt selectForInspection:in: selectMessageNode:in: selectNodeRange:in: selectNodeUnderCursorForInspectionIn: withReceiverRangeOf:in:selectorPosition:do:)
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('events' clickAndHalf)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('extract to temporary' extractToTemporary extractToTemporary:)
('extract method' extractMethod extractMethod:)
('menu' createMenuCollectingOptionsWith: getMenu openMenu:)
('menu messages' browseClassFromIt browseImplementorsAt: browseIt classCommentsContainingIt explain fileItIn implementorsOfIt implementorsOfItWhenErrorsParsing methodSourceContainingIt methodStringsContainingit paste referencesToIt referencesToSelectedLiteral selectedSelector selectedSymbol sendersOfIt sendersOfItWhenErrorsParsing withSelectorAt:do:ifBehavior:otherwise: withSelectorUnderCursorDo:ifBehavior:otherwise:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier wordLeftDelimiters wordRightDelimiters)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
('temp to inst var' temporaryToInstanceVariable temporaryToInstanceVariable: withNodeUnderCursorDo:ifAbsent:)
('typing/selecting keys' argNext: argPrev: displayIfFalse: displayIfTrue: newLine: normalCharacter:)
('private' codeProvider hasValidCurrentCompiledMethod performCodeExtractionRefactoringWith:)
('quick action (shift+click)' hoverHelpStringOfEvaluating: hoverHelpToShowEvaluating: implementorsOfNodeUnder: quickPrintOfNodeUnder:)
('inline method' contextualInlineMethod contextualInlineMethod: contextualInlineMethodOf:in: inlineMethodInUsage:)
('inline temporaray variable' inlineTemporaryVariable inlineTemporaryVariable:)
!


!FileIOAccessor reorganize!
('actions' absolutePathComponentsFor: copy:to: createDirectory: deleteDirectory: deleteFile: deleteFile:ifAbsent: fileOrDirectoryExists:in: rename:to: renameDirectory:to:)
('private' basicDirectoryExists: containingDirectoryPathOf: entriesIn: try:forFileNamed: updateEntry:)
('utilities' baseNameAndExtensionFor:do: baseNameFor: copyFile:toFile: directoryNamesIn: entryNamesIn: extensionFor: fileNamesIn: fileSize: splitNameVersionExtensionFor:)
('accessing' drives pathNameDelimiter slash)
('file stream creation' privateForceNewFile: privateNewFile: privateReadOnlyFile: privateWriteableFile:)
('testing' isCaseSensitive isDriveSupported onMacOsX onUnix onWindows)
('primitives' primCreateDirectory: primDeleteDirectory: primDeleteFileNamed: primFixedNameLookupEntryIn:index: primLookupEntryIn:index: primPathNameDelimiter primRename:to:)
!


!Utf8ReadAdaptor reorganize!
('accessing' collectionSpecies)
('reading' next next: streamUpToEnd)
!


!Clipboard reorganize!
('accessing' chooseRecentClipping contentsOriginalObject retrieveMorph retrieveObject retrieveStringOrText storeObject:)
('primitives' primitiveClipboardString primitiveClipboardString:)
('private' extendedClipboardInterface idFor: noteRecentClipping: retrieveIdOrStringFromOS stringOrIdFor:)
('initialization' initialize)
!


!DropFilesEvent reorganize!
('initialization' initializeAt:with:from:)
('testing' isDropEvent wasHandled wasHandled:)
('dispatching' dispatchWith: sendEventTo: startDispatchFrom:)
('position' eventPosition)
('primitives' primDropRequestFileHandle: primDropRequestFileName:)
('files' fileNamesDo: numberOfFiles)
!

