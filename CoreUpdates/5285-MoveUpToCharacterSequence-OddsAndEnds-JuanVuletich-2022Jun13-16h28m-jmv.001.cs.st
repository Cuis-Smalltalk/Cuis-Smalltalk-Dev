'From Cuis 6.0 [latest update: #5280] on 13 June 2022 at 4:56:18 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 16:32:53'!
base64Decoded
	"Decode the receiver from base 64"
	"
	'SGVsbG8gV29ybGQ=' base64Decoded
	'SGVsbG8gV29ybGQ=' base64Decoded asString
	"
	^(Base64MimeConverter mimeDecode: self)! !

!CharacterSequence methodsFor: 'testing' stamp: 'jmv 6/13/2022 16:37:27'!
isRemote
	^false! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 6/13/2022 16:55:29'!
asHex

	^bytes asHex! !

!Utf8String methodsFor: 'printing' stamp: 'jmv 6/13/2022 16:36:46'!
isLiteral
self flag: #unicodeNeedsImprovement.
	^super isLiteral! !


!Utf8Symbol methodsFor: 'printing' stamp: 'jmv 6/13/2022 16:36:57'!
isLiteral
self flag: #unicodeNeedsImprovement.
	^super isLiteral! !


!ByteArray methodsFor: 'converting' stamp: 'jmv 6/13/2022 16:54:10'!
asHex
	"Answer an hexa decimal representation of the receiver
	
	        #[122 43 213 7] asHex
		=> '7A 2B D5 07'
	"

	^String streamContents: [ :strm |
		self
			do: [ :byte | strm
				nextPut: ('0123456789ABCDEF' at: byte//16+1);
				nextPut: ('0123456789ABCDEF' at: byte\\16+1)]
			separatedBy: [strm space ]].! !


!String methodsFor: 'converting' stamp: 'jmv 6/13/2022 16:55:56'!
asHex

	^String streamContents: [ :strm |
		self do: [ :ch | strm nextPutAll: ch hex ] separatedBy: [ strm space ]].! !

!String methodsFor: 'converting' stamp: 'jmv 6/13/2022 16:39:45'!
substrings: aByteStringOrByteArray
	"Answer an array of the substrings that compose the argument.
	If argument is an UTF-8 encoded ByteArray, answer is an array of same."

	| end beginning |
	end _ 0.
	^Array streamContents: [ :result |
		"find one substring each time through this loop"
		[ 
			"find the beginning of the next substring"
			beginning _ aByteStringOrByteArray indexOfAnyOf: CSNonSeparators startingAt: end+1 ifAbsent: nil.
			beginning notNil ] 
		whileTrue: [
			"find the end"
			end _ aByteStringOrByteArray indexOfAnyOf: CSSeparators startingAt: beginning ifAbsent: [ aByteStringOrByteArray size + 1 ].
			end _ end - 1.
			result nextPut: (aByteStringOrByteArray copyFrom: beginning to: end).
		]
	].! !


!String class methodsFor: 'primitives' stamp: 'jmv 6/13/2022 16:40:18'!
findFirstInString: aByteStringOrByteArray inSet: inclusionMap startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>
	self var: #aString declareC: 'unsigned char *aString'.
	self var: #inclusionMap  declareC: 'char *inclusionMap'.

	inclusionMap size ~= 256 ifTrue: [ ^0 ].

	i _ start.
	stringSize _ aByteStringOrByteArray size.
	[ i <= stringSize and: [ (inclusionMap at: (aByteStringOrByteArray at: i) numericValue+1) = 0 ] ] whileTrue: [ 
		i _ i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/13/2022 16:36:41'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ ('' compare: bytes with: aString) = 1].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ ('' compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) = 1! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/13/2022 16:36:25'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ ('' compare: bytes with: aString) <= 2].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ ('' compare: bytes with: aString bytes) <= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) <= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/13/2022 16:36:20'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ ('' compare: bytes with: aString) = 3].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ ('' compare: bytes with: aString bytes) = 3]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) = 3! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/13/2022 16:36:16'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ ('' compare: bytes with: aString) >= 2].
		(aString isUtf8String and: [aString isAscii]) ifTrue: [
			"aString is also an ascii Utf8String. Their bytes are also same as equiv String."
			^ ('' compare: bytes with: aString bytes) >= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) >= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/13/2022 16:36:12'!
caseInsensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^self asByteString caseInsensitiveLessOrEqual: aString ].
		(aString isUtf8String and: [ aString isAscii ]) ifTrue: [
			^self asByteString sameAs: aString asByteString ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase <= aString asLowercase! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/13/2022 16:36:09'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	self size = aString size ifFalse: [
		^false ].

	"Fast lane. Use String implementation for ASCII only instances."
	(self isAscii and: [	aString isByteString ]) ifTrue: [
		^self asByteString sameAs: aString ].
	aString isUtf8String ifTrue: [
		self isAscii = aString isAscii ifFalse: [
			^false ].
		(self isAscii and: [aString isAscii]) ifTrue: [
			^self asByteString sameAs: aString asByteString ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !

!methodRemoval: String #base64Decoded stamp: 'jmv 6/13/2022 16:32:53'!
String removeSelector: #base64Decoded!
!methodRemoval: String #isRemote stamp: 'jmv 6/13/2022 16:37:27'!
String removeSelector: #isRemote!

!Utf8String reorganize!
('accessing' at: bytes findString:startingAt: findString:startingAt:caseSensitive: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfSubCollection:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with: copyWith:)
('comparing' < <= = > >= basicHash caseInsensitiveLessOrEqual: computeHash hash is:substringAt: sameAs:)
('converting' asByteString asHex asUtf32Words asUtf8Bytes asUtf8String substrings surroundedBySingleQuotes withLineEndings:)
('enumerating' do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' is: isAscii isEmpty isUtf8String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('printing' isLiteral)
('private' boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: privateBytes:codePointCount: stride)
('as yet unclassified' doesNotUnderstand: numArgs readStream)
('initialization' initialize)
!

