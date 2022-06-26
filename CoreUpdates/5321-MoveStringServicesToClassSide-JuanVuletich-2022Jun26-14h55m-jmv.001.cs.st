'From Cuis 6.0 [latest update: #5320] on 26 June 2022 at 3:25:27 pm'!

!String class methodsFor: 'services' stamp: 'jmv 6/26/2022 15:06:47'!
compare: string1 with: string2
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	^self compare: string1 with: string2 collated: CaseSensitiveOrder! !

!String class methodsFor: 'services' stamp: 'ar 2/3/2001 16:12'!
compare: string1 with: string2 collated: order
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	<primitive: 'primitiveCompareString' module: 'MiscPrimitivePlugin'>
	self var: #string1 declareC: 'unsigned char *string1'.
	self var: #string2 declareC: 'unsigned char *string2'.
	self var: #order declareC: 'unsigned char *order'.

	len1 _ string1 size.
	len2 _ string2 size.
	1 to: (len1 min: len2) do:
		[:i |
		c1 _ order at: (string1 basicAt: i) + 1.
		c2 _ order at: (string2 basicAt: i) + 1.
		c1 = c2 ifFalse: 
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
! !

!String class methodsFor: 'services' stamp: 'jmv 6/26/2022 15:16:59'!
findStringIgnoreCase: subString in: body startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0.
	Ignore case"

	^ self findSubstring: subString in: body startingAt: start matchTable: CaseInsensitiveOrder.! !

!String class methodsFor: 'services' stamp: 'jmv 6/26/2022 15:09:23'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	self var: #key declareC: 'unsigned char *key'.
	self var: #body declareC: 'unsigned char *body'.
	self var: #matchTable declareC: 'unsigned char *matchTable'.

	key isEmpty ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index _ 1.
			[(matchTable at: (body at: startIndex+index-1) numericValue + 1)
				= (matchTable at: (key at: index) numericValue + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index _ index+1]].
	^ 0
"
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
String findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
String findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"! !

!String class methodsFor: 'services' stamp: 'jmv 6/26/2022 15:05:49'!
is: aStringOrByteArray equalTo: another
	"Take adventage of String primitive for comparing, for example, ByteArrays"

	^ (self compare: aStringOrByteArray with: another collated: CaseSensitiveOrder) = 2! !

!String class methodsFor: 'services' stamp: 'jmv 6/26/2022 15:00:28'!
string: aString lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| start end endWithoutDelimiters |
	start _ 1.
	[
		end _ aString indexOfAnyOf: CSLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [
				"Last line was found. Evaluate and exit.
				Note. If last char in receiver is a line separator, there's an extra empty line"
				endWithoutDelimiters _ end _ aString size.
				aBlock value: start value: endWithoutDelimiters value: end.
				^aString ].

		"Account for CrLf sequence"
		endWithoutDelimiters _ end - 1.
		(end < aString size
			and: [(aString basicAt: end + 1) = `Character lf codePoint`
			and: [(aString basicAt: end) = `Character cr codePoint` ]])
				ifTrue: [ end _ end + 1].

		aBlock value: start value: endWithoutDelimiters value: end.
		start _ end + 1 ] repeat! !

!String class methodsFor: 'services' stamp: 'jmv 6/26/2022 14:59:39'!
string: aString withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"
	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |
	cr _ Character cr.
	lf _ Character lf.
	crlf _ CharacterSet new.
	crlf add: cr; add: lf.

	inPos _ 1.
	outPos _ 1.
	lineEndingSize _ lineEndingString size.
	"This could be extremely inefficient if lineEndingString size > 1"
	outString _ aString class new: aString size * lineEndingSize.

	[
		lineEndPos _ aString indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos _ outPos + (lineEndPos - inPos).
			outString replaceFrom: outPos to: newOutPos - 1 with: aString startingAt: inPos.
			1 to: lineEndingSize do: [ :i |
				outString at: newOutPos put: (lineEndingString at: i).
				newOutPos _ newOutPos + 1 ].
			outPos _ newOutPos.
			((aString at: lineEndPos) = cr and: [ lineEndPos < aString size and: [ (aString at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos _ lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos _ lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos _ outPos + (aString size - inPos).
	outString replaceFrom: outPos to: newOutPos with: aString startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos! !

!String class methodsFor: 'services' stamp: 'jmv 6/26/2022 14:57:27'!
substringsIn: aByteStringOrByteArray
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

!String class methodsFor: 'accessing' stamp: 'di 11/15/1998 16:43'!
findString: subString in: body startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ self findSubstring: subString in: body startingAt: start matchTable: CaseSensitiveOrder.! !


!String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:11:24'!
findString: subString startingAt: start 
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	subString isByteString ifFalse: [
		(subString isUtf8String and: [subString isAscii])  "Use cheapest tests and conversion possible."
			ifTrue: [ ^self findString: subString asByteString startingAt: start  ].
		^self asUtf8String findString: subString startingAt: start ].
	^ String findString: subString in: self startingAt: start! !

!String methodsFor: 'accessing' stamp: 'di 11/15/1998 16:45'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	^caseSensitive
		ifTrue: [String findSubstring: key in: self startingAt: start matchTable: CaseSensitiveOrder]
		ifFalse: [String findSubstring: key in: self startingAt: start matchTable: CaseInsensitiveOrder]! !

!String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:11:44'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock
	| index |
	subString isByteString ifFalse: [
		(subString isUtf8String and: [subString isAscii])  "Use cheapest tests and conversion possible."
			ifTrue: [ ^self indexOfSubCollection: subString asByteString
						startingAt: start ifAbsent: exceptionBlock ].
		^self asUtf8String indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock ].
	index _ String findString: subString in: self startingAt: start.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:00:38'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	String string: self lineIndicesDo: aBlock! !

!String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:02:08'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString > self ].

	^ (String compare: self with: aString collated: CaseSensitiveOrder) = 1! !

!String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:02:12'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString >= self ].

	^ (String compare: self with: aString collated: CaseSensitiveOrder) <= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:02:17'!
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

	^ (String compare: self with: aString collated: CaseSensitiveOrder) = 2.! !

!String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:02:22'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString < self ].

	^ (String compare: self with: aString collated: CaseSensitiveOrder) = 3! !

!String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:02:26'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString <= self ].

	^ (String compare: self with: aString collated: CaseSensitiveOrder) >= 2! !

!String methodsFor: 'comparing' stamp: 'sw 4/19/1999 12:11'!
caseInsensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case insensitive."

	^ (String compare: self with: aString collated: CaseInsensitiveOrder) <= 2! !

!String methodsFor: 'comparing' stamp: 'di 2/27/98 12:41'!
compare: aString 
	"Answer a comparison code telling how the receiver sorts relative to aString:
		1 - before
		2 - equal
		3 - after.
	The collation sequence is ascii with case differences ignored.
	To get the effect of a <= b, but ignoring case, use (a compare: b) <= 2."

	^ String compare: self with: aString collated: CaseInsensitiveOrder! !

!String methodsFor: 'comparing' stamp: 'ar 4/10/2005 16:42'!
compare: aString caseSensitive: aBool
	"Answer a comparison code telling how the receiver sorts relative to aString:
		1 - before
		2 - equal
		3 - after.
	"
	| map |
	map := aBool ifTrue:[CaseSensitiveOrder] ifFalse:[CaseInsensitiveOrder].
	^String compare: self with: aString collated: map! !

!String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:11:50'!
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
	^(String findString: subString in: self startingAt: index) = index! !

!String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:03:56'!
sameAs: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString sameAs: self ].

	^ (String compare: self with: aString collated: CaseInsensitiveOrder) = 2! !

!String methodsFor: 'converting' stamp: 'jmv 6/26/2022 14:57:43'!
substrings
	"Answer an array of the substrings that compose the receiver."
	^String substringsIn: self! !

!String methodsFor: 'converting' stamp: 'jmv 6/26/2022 14:59:49'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	^ String string: self withLineEndings: lineEndingString! !

!String methodsFor: 'system primitives' stamp: 'jmv 6/26/2022 15:18:45'!
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
			findSubstring: '~'
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


!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:12:03'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	(self isAscii and: [subString isByteString])
		ifTrue: [ "Fast lane"
			^String findString: subString in: bytes startingAt: start ]
		ifFalse: [
			arg _ subString asUtf8BytesOrByteString.
			"Do UTF-8 encoding as needed."
			startByteIndex _ self byteIndexAt: start.
			byteIndex _ String findString: arg in: bytes startingAt: startByteIndex.
			^self codePointIndexAt: byteIndex].! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:17:16'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	caseSensitive ifTrue: [
		^self findString: key startingAt: start ].

	self isAscii ifTrue: [
		key isAscii
			ifTrue: [ "Fast lane"
				^String findStringIgnoreCase: key in: bytes startingAt: start ]
			ifFalse: [ "Won't be found"
				^0 ]].

"caseSensitive = false is very expensive. Do better."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase findString: key asLowercase startingAt: start! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:12:14'!
indexOf: anUnicodeCodePoint startingAt: start ifAbsent: aBlock
	| codePointBytes startByteIndex foundByteIndex answer |
	anUnicodeCodePoint isCharacter ifFalse: [ ^ aBlock value ].
	codePointBytes _ Character utf8BytesOfUnicodeCodePoint: anUnicodeCodePoint codePoint.
	startByteIndex _ self byteIndexAt: start.
	foundByteIndex _ String findString: codePointBytes in: bytes startingAt: startByteIndex.
	answer _ self codePointIndexAt: foundByteIndex.
	^answer = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ answer ]! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:12:22'!
indexOfSubCollection: subString startingAt: start ifAbsent: exceptionBlock

	| index arg startByteIndex byteIndex |
	arg _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].
	startByteIndex _ self byteIndexAt: start.
	byteIndex _ String findString: arg in: bytes startingAt: startByteIndex.
	index _ self codePointIndexAt: byteIndex.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!Utf8String methodsFor: 'accessing' stamp: 'jmv 6/26/2022 15:00:45'!
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"

	String string: bytes lineIndicesDo: [ :startByte :endByteWithoutDelimiters :endByte |
		| start end endWithoutDelimiters |
		start _ self codePointIndexAt: startByte.
		end _ self codePointIndexAt: endByte.
		endWithoutDelimiters _ endByteWithoutDelimiters = endByte
			ifTrue: [end]
			ifFalse: [self codePointIndexAt: endByteWithoutDelimiters].
		aBlock
			value: start
			value: endWithoutDelimiters
			value: end
		]! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:22:12'!
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

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ (String compare: bytes with: aString asUtf8BytesOrByteString) = 1! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:22:40'!
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

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ (String compare: bytes with: aString asUtf8BytesOrByteString) <= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:06:08'!
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
			^false ]].

	"May need to do UTF-8 encoding."
	^ bytes = aString asUtf8Bytes.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:22:50'!
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

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ (String compare: bytes with: aString asUtf8BytesOrByteString) = 3! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:22:56'!
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

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
	^ (String compare: bytes with: aString asUtf8BytesOrByteString) >= 2! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:12:29'!
is: subString substringAt: index
	"Answer whether the receiver includes aString as a subcollection at position index.
	The comparison is case-sensitive."

	| sequenceSize subStringBytes byteIndex |
	index < 1 ifTrue: [ ^false ].
	sequenceSize _ subString size.
	sequenceSize = 0 ifTrue: [ ^true].
	self size - index + 1 < sequenceSize ifTrue: [ ^false ].
	"The following method uses a suboptimal algorithm (brute force pattern matching with O(n^2) worst case runtime), but the primitive in C is so fast (assuming large alphabets), that it's still worth using it instead of linear time pure smalltalk implementation. There are some obvious cases when the brute force algorithm is suboptimal, e.g. when the first elements don't match, so let's compare them here before using the primitive."
	subStringBytes _ subString asUtf8Bytes.
	subStringBytes _ (self isAscii and: [subString isByteString])
		ifTrue: [subString] "Fast lane"
		ifFalse: [subString asUtf8BytesOrByteString].
	byteIndex _ self byteIndexAt: index.
	(bytes at: byteIndex) = (subStringBytes byteAt: 1) ifFalse: [ ^false ].
	^(String findString: subStringBytes in: bytes startingAt: byteIndex) = byteIndex! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 6/26/2022 14:57:58'!
substrings
	"
	'  Hello World!! Some  		1234 more  ' substrings
	'  Hello World!! Some  		1234 more  ' asUtf8String substrings
	"
	^ self isAscii
		ifTrue: [ (String substringsIn: bytes) collect: [ :eachBytes |
				Utf8String fromUtf8Bytes: eachBytes codePointCount: eachBytes size ]]
		ifFalse: [ (String substringsIn: bytes) collect: [ :eachBytes |
				Utf8String fromUtf8Bytes: eachBytes ]].! !

!Utf8String methodsFor: 'converting' stamp: 'jmv 6/26/2022 14:59:55'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	| correctedBytes |
	correctedBytes _ String string: bytes withLineEndings: lineEndingString asByteArray.
	"The codePointCount trick can be done because the line endings considered are all single byte UTF-8.
	If we ever consider unicode line endings taking more than one byte, this is no longer valid (pass nil instead)."
	^  Utf8String fromUtf8Bytes: correctedBytes codePointCount: self size - bytes size + correctedBytes size.! !


!ByteArray methodsFor: 'comparing' stamp: 'jmv 6/26/2022 15:06:03'!
= another
	"Make good use of String primitives"

	self == another ifTrue: [ ^ true ].
	self species == another species ifFalse: [^ false].
	^String is: self equalTo: another ! !

!methodRemoval: String #findSubstring:in:startingAt:matchTable: stamp: 'jmv 6/26/2022 15:19:40'!
String removeSelector: #findSubstring:in:startingAt:matchTable:!
!methodRemoval: String #is:equalTo: stamp: 'jmv 6/26/2022 15:06:28'!
String removeSelector: #is:equalTo:!
!methodRemoval: String #string:withLineEndings: stamp: 'jmv 6/26/2022 15:00:10'!
String removeSelector: #string:withLineEndings:!
!methodRemoval: String #findStringIgnoreCase:in:startingAt: stamp: 'jmv 6/26/2022 15:18:17'!
String removeSelector: #findStringIgnoreCase:in:startingAt:!
!methodRemoval: String #string:lineIndicesDo: stamp: 'jmv 6/26/2022 15:00:55'!
String removeSelector: #string:lineIndicesDo:!
!methodRemoval: String #compare:with:collated: stamp: 'jmv 6/26/2022 15:05:25'!
String removeSelector: #compare:with:collated:!
!methodRemoval: String #findString:in:startingAt: stamp: 'jmv 6/26/2022 15:14:37'!
String removeSelector: #findString:in:startingAt:!
!methodRemoval: String #compare:with: stamp: 'jmv 6/26/2022 15:07:19'!
String removeSelector: #compare:with:!
!methodRemoval: String #substrings: stamp: 'jmv 6/26/2022 14:58:06'!
String removeSelector: #substrings:!

!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findString:startingAt: findString:startingAt:caseSensitive: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: size)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: compare: compare:caseSensitive: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString:)
('converting' asByteString asHex asLowercase asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asUtf8BytesOrByteString capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('private' replaceFrom:to:with:startingAt:)
('system primitives' numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAscii isByteString isEmpty)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
!

