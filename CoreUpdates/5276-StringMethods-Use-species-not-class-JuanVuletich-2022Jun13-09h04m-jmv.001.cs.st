'From Cuis 6.0 [latest update: #5271] on 13 June 2022 at 9:33:19 am'!

!Utf8Symbol methodsFor: 'private' stamp: 'jmv 6/13/2022 09:19:51'!
species
	^Utf8String! !


!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 09:24:10'!
copyReplacing: rangesToNewStrings

	^self species streamContents: [ :replacementStream |
		self copyReplacing: rangesToNewStrings into: replacementStream ].! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 09:24:04'!
copyReplacing: ranges with: newString

	"Ranges must be in order, with first ranges first. If not, result is unexpected - Hernan"

	^ self species streamContents: [ :replacementStream |
		self copyReplacing: ranges with: newString into: replacementStream ]
		! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 09:27:18'!
padded: leftOrRight to: length with: char
	leftOrRight = #left ifTrue: [
		^ (self species new: (length - self size max: 0) withAll: char) , self].
	leftOrRight = #right ifTrue: [
		^ self , (self species new: (length - self size max: 0) withAll: char)].! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 09:20:05'!
capitalized
	"Answer an object like the receiver but with first character downshifted if necesary
	'mElViN' asUtf8String capitalized
	'MElViN' asUtf8String capitalized
	('will', Random next mantissaPart printString) asUtf8String asSymbol capitalized
	('Will', Random next mantissaPart printString) asUtf8String asSymbol capitalized
	"
	self isEmpty ifTrue: [^ self].
	self first isUppercase ifTrue: [ ^self ].
	^self species streamContents: [ :strm | | isFirst corrected |
		isFirst _ true.
		self do: [ :codePoint |
			isFirst
				ifTrue: [
					corrected _ codePoint asUppercase.
					isFirst _ false ]
				ifFalse: [corrected _ codePoint].
			strm nextPut: corrected ]]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 09:27:49'!
squeezedTo: n
	"
Examples:
	Do nothing:
		'This one is a rather long phrase' squeezedTo: 32
		'This one is a rather long phrase' asUtf8String squeezedTo: 32

	1-remove blanks (result can be shorter than asked):
		'This one is a rather long phrase' squeezedTo: 30
		'This one is a rather long phrase' asUtf8String squeezedTo: 30

	2-remove necessary trailing vowels
		'This one is a rather long phrase' squeezedTo: 24
		'This one is a rather long phrase' asUtf8String squeezedTo: 24

	3-truncate as needed (and add ellipsis)
		'This one is a rather long phrase' squeezedTo: 15
		'This one is a rather long phrase' asUtf8String squeezedTo: 15

	4-avoid ellipsis
		'This one is a rather long phrase' squeezedTo: 5
		'This one is a rather long phrase' asUtf8String squeezedTo: 5
	"
	| vowelCount read write i char allowedVowels str desiredSize postFix j |
	str _ self.
	desiredSize _ n.
	str size <= n ifTrue: [^str].
	str _ str asCamelCase.
	str size <= n ifTrue: [^str].
	postFix _ ''.
	desiredSize _ n - postFix size.
	vowelCount _ str
		inject: 0
		into: [:prev :each | each isVowel ifTrue: [prev + 1] ifFalse: [prev]].
	str size - vowelCount <= desiredSize
		ifTrue: [allowedVowels _ vowelCount - (str size - desiredSize)]
		ifFalse: [
			allowedVowels _ 0.
			postFix _ '...'.
			n - postFix size < 5 ifTrue: [postFix _ ''].
			desiredSize _ n - postFix size].

	read _ str readStream.
	write _ self species writeStream.
	i _ 0.
	j _ 0.
	[read atEnd not and: [j < desiredSize]] whileTrue: [
		char _ read next.
		(char isVowel not or: [i < allowedVowels]) ifTrue: [
			char isVowel ifTrue: [i := i + 1].
			write nextPut: char.
			j _ j + 1]].
	str _ write contents , postFix.
	^ str! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 09:28:02'!
uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary
	'MElViN' asUtf8String uncapitalized
	'mElViN' asUtf8String uncapitalized
	('Will', Random next mantissaPart printString) asUtf8String asSymbol uncapitalized
	('will', Random next mantissaPart printString) asUtf8String asSymbol uncapitalized
	"
	self isEmpty ifTrue: [^ self].
	self first isLowercase ifTrue: [ ^self ].
	^self species streamContents: [ :strm | | isFirst corrected |
		isFirst _ true.
		self do: [ :codePoint |
			isFirst
				ifTrue: [
					corrected _ codePoint asLowercase.
					isFirst _ false ]
				ifFalse: [corrected _ codePoint].
			strm nextPut: corrected ]]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/13/2022 09:28:22'!
withBlanksCondensed
	"Return a copy of the receiver with leading/trailing blanks removed
	 and consecutive white spaces condensed as a single space."

	| trimmed lastWasBlank |
	trimmed _ self withBlanksTrimmed.
	^self species streamContents: [ :stream |
		lastWasBlank _ false.
		trimmed do: [ :c |
			c isSeparator
				ifTrue: [ lastWasBlank ifFalse: [ stream space ]]
				ifFalse: [ stream nextPut: c ].
			lastWasBlank _ c isSeparator ]].

	"
	' abc  d   ' withBlanksCondensed
	' abc  d   ' asUtf8String withBlanksCondensed
	' abc  d
	s
	as   zz 	q 			q' withBlanksCondensed
	' abc  d
	s
	as   zz 	q 			q' asUtf8String withBlanksCondensed
	"! !

!CharacterSequence methodsFor: 'enumerating' stamp: 'jmv 6/13/2022 09:21:00'!
collect: aBlock
	"Refer to the comment in Collection >> #collect:."
	| array |
	array _ self collectInArray: aBlock.
	(array allSatisfy: [ :c | c isCharacter ]) ifFalse: [
		^array ].
	^self species newFrom: array! !

!CharacterSequence methodsFor: 'private' stamp: 'jmv 6/13/2022 09:25:45'!
getEnclosedExpressionFrom: aStream
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"

	| result currentChar |
	result _ self species writeStream.
	[aStream atEnd 
		or: [(currentChar _ aStream next) = $}]]
			whileFalse: [result nextPut: currentChar].
	^ result contents withBlanksTrimmed.! !

!CharacterSequence methodsFor: 'formatting' stamp: 'jmv 6/13/2022 09:25:19'!
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
	^self species streamContents: [ :out |
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


!Utf8Symbol reorganize!
('accessing' precedence)
('comparing' =)
('converting' asPlainString asString asSymbol capitalized uncapitalized)
('testing' isInitializePvtSelector isKeyword isPvtSelector isSymbol)
('printing' print storeOn:)
('uCompletion' separateKeywords)
('private' species)
!

