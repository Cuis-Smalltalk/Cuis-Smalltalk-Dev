'From Cuis 6.0 [latest update: #5271] on 13 June 2022 at 8:52:23 am'!

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 6/13/2022 08:40:10'!
findAnySubStr: delimiters startingAt: start
	"Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters.  delimiters is an Array of Strings (Characters are permitted also).  If the receiver does not contain any of the delimiters, answer size + 1."

	| min ind |
	min _ self size + 1.
	delimiters do: [:delim |	"May be a char, a string of length 1, or a substring"
		delim isCharacter
			ifTrue: [ind _ self indexOfSubCollection: (String with: delim) 
						startingAt: start ifAbsent: [min]]
			ifFalse: [ind _ self indexOfSubCollection: delim 
						startingAt: start ifAbsent: [min]].
			min _ min min: ind].
	^ min! !

!CharacterSequence methodsFor: 'accessing' stamp: 'dew 11/15/2000 15:02'!
findBetweenSubStrs: delimiters
	"Answer the collection of String tokens that result from parsing self.  Tokens are separated by 'delimiters', which can be a collection of Strings, or a collection of Characters.  Several delimiters in a row are considered as just one separation."

	| tokens keyStart keyStop |
	tokens _ OrderedCollection new.
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipAnySubStr: delimiters startingAt: keyStop.
		keyStop _ self findAnySubStr: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!CharacterSequence methodsFor: 'accessing' stamp: 'nice 10/5/2009 03:15'!
findLastOccurrenceOfString: subString startingAt: start 
	"Answer the index of the last occurrence of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0.  Case-sensitive match used."

	| last now |
	last := self findString: subString startingAt: start.
	last = 0 ifTrue: [^ 0].
	[last > 0] whileTrue:
		[now := last.
		last := self findString: subString startingAt: last + 1].

	^ now
! !

!CharacterSequence methodsFor: 'accessing' stamp: 'ar 4/11/1999 22:15'!
findString: subString
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."
	^self findString: subString startingAt: 1.! !

!CharacterSequence methodsFor: 'accessing' stamp: 'tk 6/19/97 10:05'!
findTokens: delimiters keep: keepers
	"Answer the collection of tokens that result from parsing self.  The tokens are seperated by delimiters, any of a string of characters.  If a delimiter is also in keepers, make a token for it.  (Very useful for carriage return.  A sole return ends a line, but is also saved as a token so you can see where the line breaks were.)"

	| tokens keyStart keyStop |
	tokens _ OrderedCollection new.
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipDelimiters: delimiters startingAt: keyStop.
		keyStop to: keyStart-1 do: [:ii | 
			(keepers includes: (self at: ii)) ifTrue: [
				tokens add: (self copyFrom: ii to: ii)]].	"Make this keeper be a token"
		keyStop _ self findDelimiters: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 6/13/2022 08:40:46'!
skipAnySubStr: delimiters startingAt: start 
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."
	| any this ind ii |
	ii _ start-1.
	[(ii _ ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		any _ false.
		delimiters do: [ :delim |
			delim isCharacter
				ifTrue: [ (self at: ii) = delim ifTrue: [any _ true]]
				ifFalse: ["a substring"
					delim size > (self size - ii + 1) ifFalse: [ "Here's where the one-off error was."
						ind _ 0.
						this _ true.
						delim do: [ :dd | 
							dd = (self at: ii+ind) ifFalse: [this _ false].
							ind _ ind + 1].
						this ifTrue: [ii _ ii + delim size - 1.  any _ true]]
							ifTrue: [any _ false] "if the delim is too big, it can't match"]].
		any ifFalse: [^ ii]].
	^ self size + 1! !

!CharacterSequence methodsFor: 'private' stamp: 'jmv 6/13/2022 08:50:22'!
evaluateExpression: aString parameters: aCollection 
	"private - evaluate the expression aString with  
	aCollection as the parameters and answer the  
	evaluation result as an string"

	| index |
	index _ ('0' , aString) asNumber.
	index isZero
		ifTrue: [^ '[invalid subscript: {1}]' format: {aString}].
	index > aCollection size
		ifTrue: [^ '[subscript is out of bounds: {1}]' format: {aString}].
	^ (aCollection at: index) asString! !

!CharacterSequence methodsFor: 'private' stamp: 'jmv 6/13/2022 08:51:35'!
getEnclosedExpressionFrom: aStream
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"

	| result currentChar |
	result _ self class writeStream.
	[aStream atEnd 
		or: [(currentChar _ aStream next) = $}]]
			whileFalse: [result nextPut: currentChar].
	^ result contents withBlanksTrimmed.! !

!CharacterSequence methodsFor: 'formatting' stamp: 'jmv 6/13/2022 08:50:33'!
format: aCollection 
	"format the receiver with aCollection  
	 
	simplest example:
	'foo {1} bar' format: {Date today}.
	'foo {1} bar' asUtf8String format: {Date today}.
	 
	complete example:
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}.
	'\{ \} \\ foo {1} bar {2}' asUtf8String format: {12. 'string'}.
	"
	| stream |
	^self class streamContents: [ :out |
		stream _ self readStream.
		[stream atEnd]
			whileFalse: [| currentChar | 
				currentChar _ stream next.
				currentChar = ${
					ifTrue: [| expression | 
						expression _ self getEnclosedExpressionFrom: stream.
						out nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
					ifFalse: [
						currentChar = $\
							ifTrue: [stream atEnd ifFalse: [out nextPut: stream next]]
							ifFalse: [out nextPut: currentChar]]]].! !

!methodRemoval: String #format: stamp: 'jmv 6/13/2022 08:48:17'!
String removeSelector: #format:!
!methodRemoval: String #skipAnySubStr:startingAt: stamp: 'jmv 6/13/2022 08:40:46'!
String removeSelector: #skipAnySubStr:startingAt:!
!methodRemoval: String #findLastOccurrenceOfString:startingAt: stamp: 'jmv 6/13/2022 08:42:07'!
String removeSelector: #findLastOccurrenceOfString:startingAt:!
!methodRemoval: String #findString: stamp: 'jmv 6/13/2022 08:42:28'!
String removeSelector: #findString:!
!methodRemoval: String #evaluateExpression:parameters: stamp: 'jmv 6/13/2022 08:50:06'!
String removeSelector: #evaluateExpression:parameters:!
!methodRemoval: String #findAnySubStr:startingAt: stamp: 'jmv 6/13/2022 08:39:20'!
String removeSelector: #findAnySubStr:startingAt:!
!methodRemoval: String #findTokens:keep: stamp: 'jmv 6/13/2022 08:44:58'!
String removeSelector: #findTokens:keep:!
!methodRemoval: String #findBetweenSubStrs: stamp: 'jmv 6/13/2022 08:39:51'!
String removeSelector: #findBetweenSubStrs:!
!methodRemoval: String #getEnclosedExpressionFrom: stamp: 'jmv 6/13/2022 08:46:04'!
String removeSelector: #getEnclosedExpressionFrom:!
