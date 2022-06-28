'From Cuis 6.0 [latest update: #5328] on 28 June 2022 at 2:59:23 pm'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 6/28/2022 12:41:58'!
findString: subString startingAt: start caseSensitive: caseSensitive
	
	^caseSensitive
		ifTrue: [ self findString: subString startingAt: start ]
		ifFalse: [ self findStringCaseInsensitive: subString startingAt: start ]! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 6/28/2022 14:32:01'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| index |
	subString isString ifFalse: [
		^super indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock ].
	index _ self findString: subString startingAt: start.
	index = 0 ifTrue: [ ^exceptionBlock value ].
	^index! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 6/28/2022 14:28:48'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true ].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	1 to: sequenceSize do: [ :i |
		(self at: index+i-1) = (subString at: i) ifFalse: [ ^false ]].
	^true! !


!String methodsFor: 'accessing' stamp: 'jmv 6/28/2022 12:39:30'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"An ASCII Utf8String is has same bytes as its String equivalent"
	(subString isUtf8String and: [ subString isAscii ]) ifTrue: [
		^String findStringIgnoreCase: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^self asUtf8String findStringCaseInsensitive: subString startingAt: start ].
	"Quick primitive if both String"
	^String findStringIgnoreCase: subString in: self startingAt: start! !


!String class methodsFor: 'services' stamp: 'jmv 6/28/2022 12:17:17'!
compareIgnoreCase: string1 with: string2
	"Return 1, 2 or 3, if string1 is <, =, or > string2, 
	with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: CaseInsensitiveOrder! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/28/2022 12:39:39'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findStringIgnoreCase: subString in: bytes startingAt: start ].
		subString isUtf8String ifTrue: [
			^subString isAscii
				ifFalse: [ false ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findStringIgnoreCase: subString bytes in: bytes startingAt: start ]]].

"caseSensitive = false is very expensive. Do better."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase findString: subString asLowercase startingAt: start! !


!Text methodsFor: 'accessing' stamp: 'jmv 6/28/2022 12:40:43'!
findStringCaseInsensitive: aString startingAt: start
	"Answer the index of subString within the receiver, starting at position start.
	If the receiver does not contain subString, answer 0."

	^string findStringCaseInsensitive: aString asPlainString startingAt: start! !


!String methodsFor: 'accessing' stamp: 'jmv 6/28/2022 12:02:25'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	"An ASCII Utf8String is has same bytes as its String equivalent"
	(subString isUtf8String and: [subString isAscii]) ifTrue: [
		^ String findString: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^ self asUtf8String findString: subString startingAt: start ].
	"Quick primitive if both String"
	^ String findString: subString in: self startingAt: start! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 12:49:45'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString > self ].

	^ (String compare: self with: aString) = 1! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 12:49:40'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString >= self ].

	^ (String compare: self with: aString) <= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 14:37:09'!
= aString
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol or Utf32String."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String or Symbol, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Let them handle us"
	aString isByteString ifFalse: [
		^aString = self ].

	^String is: self equalTo: aString.! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 12:49:26'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString < self ].

	^ (String compare: self with: aString) = 3! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 12:49:21'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString <= self ].

	^ (String compare: self with: aString) >= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 14:58:45'!
caseInsensitiveLessOrEqual: aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case insensitive."

	"An ASCII Utf8String is has same bytes as its String equivalent"
	(aString isUtf8String and: [ aString isAscii ]) ifTrue: [
		^(String compareIgnoreCase: self with: aString bytes) <= 2 ].
	"Do conversions if needed"
	aString isByteString ifFalse: [
		^self asUtf8String caseInsensitiveLessOrEqual: aString ].
	"Quick primitive if both String"
	^ (String compareIgnoreCase: self with: aString) <= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 14:29:08'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].

	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self at: index) = (subString at: 1) ifFalse: [ ^false ].
	sequenceSize = 1 ifTrue: [ ^true ].
	(self at: index+sequenceSize-1) = (subString at: sequenceSize) ifFalse: [ ^false ].
	sequenceSize = 2 ifTrue: [ ^true ].
	(subString isAscii not and: [self isAscii]) ifTrue: [ ^false ].

	"Try to find arguments for the primitive in #findString:in:startingAt: cheaply"
	subStringBytes _ nil.
	subString isByteString ifTrue: [
		subStringBytes _ subString ].
	(subString isUtf8String and: [ subString isAscii ]) ifTrue: [
		"Can use #bytes, because it has just ASCII bytes"
		subStringBytes _ subString bytes ].
	subStringBytes ifNotNil: [
		^(String findString: subStringBytes in: self startingAt: index) = index ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to String"
	^super is: subString substringAt: index! !

!String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 12:17:47'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"An ASCII Utf8String is has same bytes as its String equivalent"
	(aString isUtf8String and: [aString isAscii]) ifTrue: [
		^ (String compareIgnoreCase: self with: aString bytes) = 2 ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString sameAs: self ].

	^ (String compareIgnoreCase: self with: aString) = 2! !

!String methodsFor: 'system primitives' stamp: 'jmv 6/28/2022 12:54:22'!
numArgs
	"Answer either the number of arguments that the receiver would take if considered a selector. 
	Answer -1 if it couldn't be a selector. 
		Note that currently this will answer -1 for anything begining with an uppercase letter even though 
		the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."
	| firstChar numColons start ix |
	self isEmpty ifTrue: [ ^ -1 ].
	"Binary messages"
	(self allSatisfy: [ :c | c isValidInBinarySelectors ])
		ifTrue: [ ^1 ].
	"Unary and keyword messages"
	firstChar _ self at: 1.
	firstChar isValidStartOfIdentifiers ifTrue: [
		"Fast reject if any chars are non-alphanumeric"
		(String
			findSubstring: `'~' asByteString`
			in: self
			startingAt: 1
			matchTable: Tokenish) > 0 ifTrue: [ ^ -1 ].
		"Fast colon count"
		numColons _ 0.
		start _ 1.
		[ (ix _ self findString: ':' startingAt: start) > 0 ] whileTrue: [
			numColons _ numColons + 1.
			start _ ix + 1 ].
		^ numColons ].
	^ -1.! !


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/28/2022 13:00:23'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findString: subString in: bytes startingAt: start ].
		subString isUtf8String ifTrue: [
			^subString isAscii
				ifFalse: [ false ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findString: subString bytes in: bytes startingAt: start ]]].

	arg _ subString asUtf8BytesOrByteString.
	"Do UTF-8 encoding as needed."
	startByteIndex _ self byteIndexAt: start.
	byteIndex _ String findString: arg in: bytes startingAt: startByteIndex.
	^self codePointIndexAt: byteIndex! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 14:40:35'!
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
		^String is: bytes equalTo: aString ].
	
	aString isUtf8String ifTrue: [
		self isAscii = aString isAscii ifFalse: [
			^false ].
		^String is: bytes equalTo: aString bytes ].

	^ self beginsWith: aString.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 14:58:48'!
caseInsensitiveLessOrEqual: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^(String compareIgnoreCase: bytes with: aString) <= 2 ].
		(aString isUtf8String and: [ aString isAscii ]) ifTrue: [
			^(String compareIgnoreCase: bytes with: aString bytes) <= 2 ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase <= aString asLowercase! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 14:29:18'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes byteIndex subStringIsAscii |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].

	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	(self at: index) = (subString at: 1) ifFalse: [ ^false ].
	sequenceSize = 1 ifTrue: [ ^true ].
	(self at: index+sequenceSize-1) = (subString at: sequenceSize) ifFalse: [ ^false ].
	sequenceSize = 2 ifTrue: [ ^true ].
	subStringIsAscii _ subString isAscii.
	(self isAscii and: [subStringIsAscii not]) ifTrue: [ ^false ].

	"Try to find arguments for the primitive in #findString:in:startingAt: cheaply"
	subStringBytes _ nil.
	subString isUtf8String ifTrue: [
		"Simple. Take the bytes"
		subStringBytes _ subString bytes ].
	(subString isByteString and: [ subStringIsAscii ]) ifTrue: [
		"Can use instance of String, because it has just ASCII bytes"
		subStringBytes _ subString ].
	subStringBytes ifNotNil: [
		byteIndex _ self byteIndexAt: index.
		^(String findString: subStringBytes in: bytes startingAt: byteIndex) = byteIndex ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to Utf8String"
	^super is: subString substringAt: index! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/28/2022 12:17:57'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	self size = aString size ifFalse: [
		^false ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^(String compareIgnoreCase: bytes with: aString) = 2 ].
		aString isUtf8String ifTrue: [
			^ aString isAscii
				ifFalse: [ false ] "One is ASCII, other isn't."
				ifTrue: [
					"Both are ASCII"
					(String compareIgnoreCase: bytes with: aString bytes) = 2 ]]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !

!methodRemoval: Utf8String #indexOfSubCollection:startingAt:ifAbsent: stamp: 'jmv 6/28/2022 14:57:59'!
Utf8String removeSelector: #indexOfSubCollection:startingAt:ifAbsent:!
!methodRemoval: Utf8String #findString:startingAt:caseSensitive: stamp: 'jmv 6/28/2022 14:57:59'!
Utf8String removeSelector: #findString:startingAt:caseSensitive:!
!methodRemoval: String #indexOfSubCollection:startingAt:ifAbsent: stamp: 'jmv 6/28/2022 14:57:59'!
String removeSelector: #indexOfSubCollection:startingAt:ifAbsent:!
!methodRemoval: String #findString:startingAt:caseSensitive: stamp: 'jmv 6/28/2022 14:57:59'!
String removeSelector: #findString:startingAt:caseSensitive:!
