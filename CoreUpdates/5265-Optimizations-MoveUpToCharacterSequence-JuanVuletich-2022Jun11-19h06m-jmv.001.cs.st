'From Cuis 6.0 [latest update: #5263] on 11 June 2022 at 8:49:54 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'di 11/15/1998 16:53'!
includesSubstring: aString caseSensitive: caseSensitive
	
	^ (self findString: aString startingAt: 1 caseSensitive: caseSensitive) > 0! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 6/11/2022 19:15:43'!
indexOf: aCharacter

	^self indexOf: aCharacter startingAt: 1! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 6/11/2022 16:21:15'!
indexOfAnyOf: aCharacterSet
	"returns the index of the first character in the given set, starting from start"

	^ self indexOfAnyOf: aCharacterSet  startingAt: 1 ifAbsent: 0! !

!CharacterSequence methodsFor: 'comparing' stamp: 'di 11/19/1998 13:37'!
match: text
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."

	^ self startingAt: 1 match: text startingAt: 1
"
	'*'			match: 'zort' true
	'*baz'		match: 'mobaz' true
	'*baz'		match: 'mobazo' false
	'*baz*'		match: 'mobazo' true
	'*baz*'		match: 'mozo' false
	'foo*'		match: 'foozo' true
	'foo*'		match: 'bozo' false
	'foo*baz'	match: 'foo23baz' true
	'foo*baz'	match: 'foobaz' true
	'foo*baz'	match: 'foo23bazo' false
	'foo'		match: 'Foo' true
	'foo*baz*zort' match: 'foobazort' false
	'foo*baz*zort' match: 'foobazzort' false
	'*foo#zort'	match: 'afoo3zortthenfoo3zort' true
	'*foo*zort'	match: 'afoodezortorfoo3zort' true
"! !

!CharacterSequence methodsFor: 'comparing' stamp: 'ul 11/14/2010 06:02'!
startingAt: keyStart match: text startingAt: textStart
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."
	| anyMatch matchStart matchEnd i matchStr j ii jj |
	i := keyStart.
	j := textStart.

	"Check for any #'s"
	[i > self size ifTrue: [^ j > text size "Empty key matches only empty string"].
	(self at: i) = $#] whileTrue:
		["# consumes one char of key and one char of text"
		j > text size ifTrue: [^ false "no more text"].
		i := i+1.  j := j+1].

	"Then check for *"
	(self at: i) = $*
		ifTrue: [i = self size ifTrue:
					[^ true "Terminal * matches all"].
				"* means next match string can occur anywhere"
				anyMatch := true.
				matchStart := i + 1]
		ifFalse: ["Otherwise match string must occur immediately"
				anyMatch := false.
				matchStart := i].

	"Now determine the match string"
	matchEnd := self size.
	(ii := self indexOf: $* startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '** not valid -- use * instead'].
		matchEnd := ii-1].
	(ii := self indexOf: $# startingAt: matchStart) > 0 ifTrue:
		[ii = matchStart ifTrue: [self error: '*# not valid -- use #* instead'].
		matchEnd := matchEnd min: ii-1].
	matchStr := self copyFrom: matchStart to: matchEnd.

	"Now look for the match string"
	[jj := text findString: matchStr startingAt: j caseSensitive: false.
	anyMatch ifTrue: [jj > 0] ifFalse: [jj = j]]
		whileTrue:
		["Found matchStr at jj.  See if the rest matches..."
		(self startingAt: matchEnd+1 match: text startingAt: jj + matchStr size) ifTrue:
			[^ true "the rest matches -- success"].
		"The rest did not match."
		anyMatch ifFalse: [^ false].
		"Preceded by * -- try for a later match"
		j := j+1].
	^ false "Failed to find the match string"! !


!String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 16:11:05'!
findStringIgnoreCase: subString in: body startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0.
	Ignore case"

	^ self findSubstring: subString in: body startingAt: start matchTable: CaseInsensitiveOrder.! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 16:29:52'!
indexOf: anUnicodeCodePoint  startingAt: start  ifAbsent: aBlock
	| startByte codePointBytes answer foundByte |
	anUnicodeCodePoint isCharacter ifFalse: [ ^ aBlock value ].
	startByte _ self byteIndexAt: start.
	codePointBytes _ Character utf8BytesOfUnicodeCodePoint: anUnicodeCodePoint codePoint.
	foundByte _ '' findString: codePointBytes in: bytes startingAt: startByte.
	answer _ self codePointIndexAt: foundByte.
	^answer = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ answer ]! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 6/11/2022 19:30:22'!
asByteString
	"Answer an instance of the Byte oriented String class"

	"Fast lane."
	self isAscii ifTrue: [
		^bytes asString ].

	"Need to do UTF-8 decoding and possible NCR generation."
	^super asByteString! !


!ByteArray methodsFor: 'accessing' stamp: 'jmv 6/11/2022 19:14:09'!
indexOf: anInteger startingAt: start ifAbsent: aBlock

	| ans |
	(anInteger isInteger and: [
		anInteger >= 0 and: [
		anInteger <= 255 ] ]) ifFalse: [ ^0 ].
	ans _ String indexOfByte: anInteger inString: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/11/2022 19:39:00'!
asUtf32String
	"Answer an instance of the UTF-32 encoded String class"
self flag: #slowUnicode.
	^Utf32String fromUtf8Bytes: self asUtf8Bytes.! !


!String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 19:46:54'!
findString: subString startingAt: start 
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	subString isByteString ifFalse: [
		(subString isUtf8String and: [subString isAscii])  "Use cheapest tests and conversion possible."
			ifTrue: [ ^self findString: subString asByteString startingAt: start  ].
		^self asUtf8String findString: subString startingAt: start ].
	^ self findString: subString in: self startingAt: start! !

!String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 19:13:48'!
indexOf: aCharacter  startingAt: start  ifAbsent: aBlock
	| ans |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	ans _ String indexOfByte: aCharacter iso8859s15Code inString: self startingAt: start.
	^ans = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ ans ]! !

!String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 19:48:30'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| index |
	subString isByteString ifFalse: [
		(subString isUtf8String and: [subString isAscii])  "Use cheapest tests and conversion possible."
			ifTrue: [ ^self indexOfSubCollection: subString asByteString
						startingAt: start ifAbsent: exceptionBlock ].
		^self asUtf8String indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock ].
	index _ self findString: subString in: self startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 19:47:00'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	subString isByteString ifFalse: [
		(subString isUtf8String and: [subString isAscii])  "Use cheapest tests and conversion possible."
			ifTrue: [ ^self is: subString asByteString substringAt: index ].
		^self asUtf8String is: subString substringAt: index ].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self basicAt: index) = (subString basicAt: 1) ifFalse: [ ^false ].
	^(self findString: subString in: self startingAt: index) = index! !


!String class methodsFor: 'instance creation' stamp: 'jmv 6/11/2022 19:30:25'!
fromUtf8Bytes: aByteArray
	"Convert the given bytes from UTF-8 to  the internal encoding: ISO Latin 9 (ISO 8859-15).
	See #asUtf8Bytes "
	"For any unicode chars not in ISO Latin 9 (ISO 8859-15), embed an NCR.
	See http://en.wikipedia.org/wiki/Numeric_character_reference"

	"Fast lane."
	(String isAscii: aByteArray) ifTrue: [
		^aByteArray asString ].

	"Need to do UTF-8 decoding and possible NCR generation."
	^self fromUtf8Bytes: aByteArray hex: false trimLastNull: false! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 20:01:25'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg |
	arg _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].

	"Do UTF-8 encoding as needed."
	^ '' findString: arg in: bytes startingAt: start! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 16:12:20'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	caseSensitive ifTrue: [
		^self findString: key startingAt: start ].

	self isAscii ifTrue: [
		key isAscii
			ifTrue: [
				"Fast lane"
				^'' findStringIgnoreCase: key in: bytes startingAt: start ]
			ifFalse: [
				"Won't be found"
				^0 ]].

"caseSensitive = false is very expensive. Do better."
self flag: #slowUnicode.
	^self asLowercase findString: key asLowercase startingAt: start! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/11/2022 20:03:12'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| index arg |
	arg _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].
	index _ '' findString: arg in: bytes startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:36:23'!
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
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) = 1! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:36:28'!
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
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) <= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:22:58'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [
		"Fast lane."
		^'' is: bytes sameAs: aString ].
	
	aString isUtf8String ifTrue: [
		self isAscii = aString isAscii ifFalse: [
			^false ]].

	"May need to do UTF-8 encoding."
	^ '' is: bytes sameAs: aString asUtf8Bytes.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:36:33'!
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
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) = 3! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:36:37'!
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
	^ ('' compare: bytes with: aString asUtf8BytesOrByteString) >= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:01:14'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	subStringBytes _ subString asUtf8Bytes.
	subStringBytes _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].
	(bytes at: index) = (subStringBytes byteAt: 1) ifFalse: [ ^false ].
	^('' findString: subStringBytes in: bytes startingAt: index) = index! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/11/2022 20:44:31'!
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
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !

!methodRemoval: ByteArray #indexOf:startingAt: stamp: 'jmv 6/11/2022 19:14:28'!
ByteArray removeSelector: #indexOf:startingAt:!
!methodRemoval: String #match: stamp: 'jmv 6/11/2022 19:10:06'!
String removeSelector: #match:!
!methodRemoval: String #indexOf: stamp: 'jmv 6/11/2022 19:11:59'!
String removeSelector: #indexOf:!
!methodRemoval: String #includesSubstring:caseSensitive: stamp: 'jmv 6/11/2022 19:09:43'!
String removeSelector: #includesSubstring:caseSensitive:!
!methodRemoval: String #indexOf:startingAt: stamp: 'jmv 6/11/2022 19:12:41'!
String removeSelector: #indexOf:startingAt:!
!methodRemoval: String #indexOfAnyOf: stamp: 'jmv 6/11/2022 19:15:01'!
String removeSelector: #indexOfAnyOf:!
!methodRemoval: String #startingAt:match:startingAt: stamp: 'jmv 6/11/2022 19:10:13'!
String removeSelector: #startingAt:match:startingAt:!
