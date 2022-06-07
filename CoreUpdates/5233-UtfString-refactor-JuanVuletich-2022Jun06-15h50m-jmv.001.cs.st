'From Cuis 6.0 [latest update: #5230] on 6 June 2022 at 4:21:28 pm'!

!EncodedString methodsFor: 'copying' stamp: 'jmv 6/6/2022 16:06:18'!
appendToText: aText

	| textStringSize |
	textStringSize := aText string size.
	^ aText replaceFrom: textStringSize + 1
			 to: textStringSize 
			 with: self asText! !

!EncodedString methodsFor: 'comparing' stamp: 'jmv 6/6/2022 16:14:15'!
commonPartWith: aString startAt: startIndex stopAt: stopIndexRequested applying: aBlock
	"Return the size of the longest common subsequence with aString, only between startIndex and stopIndex.
	Apply aBlock to each character before comparing.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."
	| stopIndex |
	stopIndex _ stopIndexRequested min: aString size.
	startIndex
		to: stopIndex
		do: [ :i |
			(aBlock value: (self at: i)) = (aBlock value: (aString at: i)) ifFalse: [ ^ i - 1 ]].
	^ stopIndex! !

!EncodedString methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:07:16'!
asByteString
	"Answer an instance of the Byte oriented String class"

	^String fromUtf8Bytes: self asUtf8Bytes! !

!EncodedString methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:07:58'!
asPlainString
	"Answer the receiver itself.
	Not a Symbol, not a Text."
	^self! !

!EncodedString methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:08:08'!
asString
	"Answer the receiver itself."
	^self! !

!EncodedString methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:06:38'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !

!EncodedString methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:07:34'!
asUtf8Bytes
	^self subclassResponsibility! !

!EncodedString methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:16:29'!
contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize _ smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	(Utf8String withBytes: 'A clear but rather long-winded summary' asUtf8) contractTo: 18
"! !

!EncodedString methodsFor: 'enumerating' stamp: 'jmv 6/6/2022 16:13:15'!
collect: aBlock
	"Refer to the comment in Collection|collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacter ]) ifFalse: [
		^array ].
	^self class newFrom: array! !

!EncodedString methodsFor: 'enumerating' stamp: 'jmv 6/6/2022 16:13:44'!
collectInArray: aBlock
	"Refer to the comment in Collection|collect:."
	| result i |
	result _ Array new: self size.
	i _ 1.
	self do: [ :codePoint |
		result at: i put: codePoint.
		i _ i + 1 ].
	^ result.! !

!EncodedString methodsFor: 'testing' stamp: 'jmv 6/6/2022 16:19:41'!
isString
	^true! !

!EncodedString methodsFor: 'user interface' stamp: 'jmv 6/6/2022 16:17:30'!
edit

	self editLabel: self class name, ' Text Editor'! !

!EncodedString methodsFor: 'user interface' stamp: 'jmv 6/6/2022 16:18:04'!
editLabel: labelString

	(TextModel withText: self) openLabel: labelString! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 16:15:46'!
basicHash
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859 non ASCII characters are present."
	bytes size > 256 ifTrue: [ ^ bytes hashQuick ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash! !


!Utf32String methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:09:55'!
asUtf32String
	"Answer an instance of the UTF-32 encoded String class"

	^self! !


!Utf8String methodsFor: 'copying' stamp: 'jmv 6/6/2022 16:05:42'!
appendToString: aString

	^Utf8String withBytes: aString asUtf8Bytes, bytes! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 16:16:05'!
computeHash
	"Same hash value as the equivalent ASCII String, provided that all our CodePoints exist as Characters"
	| basicHash byteString |
	basicHash _ self basicHash.
	self isAscii ifTrue: [
		^basicHash ].
	"If there's a String that is equal to us (i.e. no NCRs were needed to represent us)"
	byteString _ self asByteString.
	byteString size = self size ifTrue: [
		^byteString hash ].
	"Otherwise, use a hash that doesn't need to equal that of a String"
	^basicHash
! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 6/6/2022 16:09:45'!
asUtf8String
	"Answer an instance of the UTF-8 encoded String class"

	^self! !

!Utf8String methodsFor: 'private' stamp: 'jmv 6/6/2022 16:12:02'!
boundsCheck: codePointIndex
	(self isInBounds: codePointIndex) 
		ifFalse: [ self errorSubscriptBounds: codePointIndex ]! !

!methodRemoval: Utf8String #commonPartWith:startAt:stopAt:applying: stamp: 'jmv 6/6/2022 16:14:22'!
Utf8String removeSelector: #commonPartWith:startAt:stopAt:applying:!
!methodRemoval: Utf8String #appendToText: stamp: 'jmv 6/6/2022 16:06:51'!
Utf8String removeSelector: #appendToText:!
!methodRemoval: Utf8String #at:ifAbsent: stamp: 'jmv 6/6/2022 15:57:52'!
Utf8String removeSelector: #at:ifAbsent:!
!methodRemoval: Utf8String #asByteString stamp: 'jmv 6/6/2022 16:07:46'!
Utf8String removeSelector: #asByteString!
!methodRemoval: Utf8String #last stamp: 'jmv 6/6/2022 16:02:01'!
Utf8String removeSelector: #last!
!methodRemoval: Utf8String #editLabel: stamp: 'jmv 6/6/2022 16:18:09'!
Utf8String removeSelector: #editLabel:!
!methodRemoval: Utf8String #contents stamp: 'jmv 6/6/2022 16:02:30'!
Utf8String removeSelector: #contents!
!methodRemoval: Utf8String #isString stamp: 'jmv 6/6/2022 16:19:43'!
Utf8String removeSelector: #isString!
!methodRemoval: Utf8String #edit stamp: 'jmv 6/6/2022 16:17:52'!
Utf8String removeSelector: #edit!
!methodRemoval: Utf8String #asString stamp: 'jmv 6/6/2022 16:08:12'!
Utf8String removeSelector: #asString!
!methodRemoval: Utf8String #collect: stamp: 'jmv 6/6/2022 16:13:21'!
Utf8String removeSelector: #collect:!
!methodRemoval: Utf8String #canHoldCodePoints stamp: 'jmv 6/6/2022 15:56:20'!
Utf8String removeSelector: #canHoldCodePoints!
!methodRemoval: Utf8String #asText stamp: 'jmv 6/6/2022 16:06:44'!
Utf8String removeSelector: #asText!
!methodRemoval: Utf8String #asPlainString stamp: 'jmv 6/6/2022 16:08:02'!
Utf8String removeSelector: #asPlainString!
!methodRemoval: Utf8String #computeHashForAscii stamp: 'jmv 6/6/2022 16:16:08'!
Utf8String removeSelector: #computeHashForAscii!
!methodRemoval: Utf8String #notEmpty stamp: 'jmv 6/6/2022 15:55:17'!
Utf8String removeSelector: #notEmpty!
!methodRemoval: Utf8String #collectInArray: stamp: 'jmv 6/6/2022 16:13:50'!
Utf8String removeSelector: #collectInArray:!
!methodRemoval: Utf8String #contractTo: stamp: 'jmv 6/6/2022 16:16:31'!
Utf8String removeSelector: #contractTo:!
!methodRemoval: Utf32String #from:to:do: stamp: 'jmv 6/6/2022 15:54:50'!
Utf32String removeSelector: #from:to:do:!
!methodRemoval: Utf32String #appendToText: stamp: 'jmv 6/6/2022 16:06:49'!
Utf32String removeSelector: #appendToText:!
!methodRemoval: Utf32String #at:ifAbsent: stamp: 'jmv 6/6/2022 15:57:28'!
Utf32String removeSelector: #at:ifAbsent:!
!methodRemoval: Utf32String #asByteString stamp: 'jmv 6/6/2022 16:07:48'!
Utf32String removeSelector: #asByteString!
!methodRemoval: Utf32String #last stamp: 'jmv 6/6/2022 16:02:05'!
Utf32String removeSelector: #last!
!methodRemoval: Utf32String #canHoldCodePoints stamp: 'jmv 6/6/2022 15:56:10'!
Utf32String removeSelector: #canHoldCodePoints!
!methodRemoval: Utf32String #collect: stamp: 'jmv 6/6/2022 16:13:19'!
Utf32String removeSelector: #collect:!
!methodRemoval: Utf32String #asText stamp: 'jmv 6/6/2022 16:06:41'!
Utf32String removeSelector: #asText!
!methodRemoval: Utf32String #editLabel: stamp: 'jmv 6/6/2022 16:18:07'!
Utf32String removeSelector: #editLabel:!
!methodRemoval: Utf32String #isString stamp: 'jmv 6/6/2022 16:19:45'!
Utf32String removeSelector: #isString!
!methodRemoval: Utf32String #do: stamp: 'jmv 6/6/2022 15:59:50'!
Utf32String removeSelector: #do:!
!methodRemoval: Utf32String #asPlainString stamp: 'jmv 6/6/2022 16:08:00'!
Utf32String removeSelector: #asPlainString!
!methodRemoval: Utf32String #notEmpty stamp: 'jmv 6/6/2022 15:55:21'!
Utf32String removeSelector: #notEmpty!
!methodRemoval: Utf32String #collectInArray: stamp: 'jmv 6/6/2022 16:13:52'!
Utf32String removeSelector: #collectInArray:!
!methodRemoval: Utf32String #edit stamp: 'jmv 6/6/2022 16:17:51'!
Utf32String removeSelector: #edit!
!methodRemoval: Utf32String #asString stamp: 'jmv 6/6/2022 16:08:10'!
Utf32String removeSelector: #asString!

!EncodedString reorganize!
('copying' appendToText:)
('comparing' commonPartWith:startAt:stopAt:applying:)
('converting' asByteString asPlainString asString asText asUtf8Bytes contractTo:)
('enumerating' collect: collectInArray:)
('testing' isString)
('user interface' edit editLabel:)
!


!Utf32String reorganize!
('accessing' at: isLastLineSeparator isLastSeparator size words words:)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = hash)
('converting' asUtf32String asUtf32Words asUtf8Bytes asUtf8String)
('testing' is: isEmpty isUtf32String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('as yet unclassified' doesNotUnderstand:)
!


!Utf8String reorganize!
('accessing' at: bytes bytes: isLastLineSeparator isLastSeparator size)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = basicHash computeHash hash)
('converting' asSymbol asUtf32String asUtf32Words asUtf8Bytes asUtf8String displayStringOrText string)
('enumerating' do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' is: isAscii isEmpty isUtf8String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('private' beAscii boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: stride)
('as yet unclassified' doesNotUnderstand:)
('initialization' initialize)
('printing' print printOn: storeOn:)
!

