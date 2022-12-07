'From Cuis 6.0 [latest update: #5579] on 7 December 2022 at 9:46:10 am'!

!String commentStamp: 'jmv 12/7/2022 09:34:19' prior: 0!
A String is an indexed collection of Characters. In Cuis, Characters are represented in the Latin-9 (ISO 8859-15) encoding. Each character takes 1 byte.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:

See UnicodeString.!

Smalltalk renameClassNamed: #Utf8ReadAdaptor as: #UnicodeReadAdaptor!

!Object methodsFor: 'testing' stamp: 'jmv 12/7/2022 09:16:18'!
isUnicodeString
	"Overridden to return true in UnicodeString, natch"
	^ false! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:13:10'!
asUnicodeString
	"Answer an instance of the UTF-8 encoded String class"

	| utf8Bytes |
	utf8Bytes _ self asUtf8Bytes.
	^UnicodeString fromUtf8Bytes: utf8Bytes codePointCount: self size.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:15:21'!
asUnicodeStringOrText
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	^self asUnicodeString! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:14:46'!
asUnicodeString
	"Convert me to an UnicodeString"
	
	^ self! !

!UnicodeString methodsFor: 'testing' stamp: 'jmv 12/7/2022 09:16:22'!
isUnicodeString

	^true! !


!Text methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:17:55'!
asUnicodeStringOrText
	"Answer is either an UnicodeString or a Text including an UnicodeString"

	string isUnicodeString ifTrue: [ ^self ].
	^Text string: string asUnicodeString runs: runs! !


!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 12/7/2022 09:44:14'!
useUnicodeString
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor _ UnicodeReadAdaptor new stream: self.! !


!Object methodsFor: 'testing' stamp: 'jmv 12/7/2022 09:09:02'!
isString
	"Overridden to return true in String, UnicodeString."
	^ false! !


!TextModel methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:15:45'!
basicActualContents: aTextOrString
	"Do not throw events."
	| prevContents |
	prevContents _ actualContents.
	actualContents _ aTextOrString asText asUnicodeStringOrText.
	actualContents = prevContents ifFalse: [	"Compares only characters, not attributes"
		undoRedoCommands resetToStart.
		lastEditTimeStamp _ nil ]! !


!CharacterSequence methodsFor: 'copying' stamp: 'jmv 12/7/2022 09:33:55'!
copyReplaceTokens: oldSubstring with: newSubstring
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"
	'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'
	"! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 12/7/2022 09:29:43'!
join: aCollection 
	"
	'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "

	^self species streamContents: [ :strm |
		aCollection
			do: [ :elem | strm nextPutAll: elem asString]
			separatedBy: [strm nextPutAll: self]].! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:28:12'!
commonPrefixWith: aString
	"Return the size of the longest common prefix with aString.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly.
	'this is a string' commonPrefixWith: 'this is another'
	'this is a string' commonPrefixWith: ''
	'' commonPrefixWith: 'this is another'
	't' commonPrefixWith: 'this is another'
	'txxxxxxxxx' commonPrefixWith: 'this is another'
	"
	^ self commonPartWith: aString startAt: 1 stopAt: self size applying: [ :c | c ]! !

!CharacterSequence methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:28:47'!
endsWith: suffix
	"Answer whether the tail end of the receiver is the same as suffix.
	The comparison is case-sensitive."

	^self is: suffix substringAt: self size - suffix size + 1
"
  'Elvis' endsWith: 'vis'
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:24:28'!
asCamelCase
	"Answer a new String, without any whitespace, and with words capitalized (Except for the first one)
	' how do you do? ' asCamelCase
	"
	^ self species streamContents: [ :outStream | | inStream capitalize wroteSome |
		wroteSome _ false.
		capitalize _ false.
		inStream _ self readStream.
		[ inStream atEnd ] whileFalse: [ | c |
			c _ inStream next.
			c isSeparator
				ifTrue: [ capitalize _ true ]
				ifFalse: [
					capitalize & wroteSome ifTrue: [ c _ c asUppercase ].
					outStream nextPut: c.
					wroteSome _ true.
					capitalize _ false ]]]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:24:42'!
asDate
	"Many allowed forms, see Date>>#readFrom:
		'2014/6/30' asDate.
		'70/12/30' asDate.
		'12/30/70' asDate.
		'30/12/70' asDate.
		'4/5/6' asDate.
		'15 April 1982' asDate.
	"

	^ Date fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:24:55'!
asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString |
	aString _ self select: [ :el | el isValidInIdentifiers ].
	(aString isEmpty or: [aString first isValidStartOfIdentifiers not])
		ifTrue:	 [aString _ 'a', aString].
	^ shouldBeCapitalized ifTrue: [ aString capitalized ] ifFalse: [ aString uncapitalized ]

"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:25:03'!
asMonth
	"Many allowed forms, see Month>>#readFrom:
		'July 1998' asMonth.
		'1998/7' asMonth.
	"

	^ Month fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:25:16'!
asUnaccented
	"
	'Hello' asUnaccented
	'áéîôóü' asUnaccented
	"
	^(self anySatisfy: [ :c | c isAccented])
		ifFalse: [ self ]
		ifTrue: [ self collect: [ :c | c asUnaccented ]]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:25:24'!
asValidInstanceVariableName
	| answer |
	answer _ self asIdentifier: false.
	(Scanner pseudoVariableNames includes: answer)
		ifTrue: [ answer _ answer , 'x' ].
	^ answer

"
'234znak 43 ) 2' asValidInstanceVariableName
'234 xx\ Uml /ler42342380-4' asValidInstanceVariableName
'super' asValidInstanceVariableName
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:25:29'!
asValidSelector
	^ self asIdentifier: false

"
'234znak 43 ) 2' asValidSelector
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:26:07'!
asWeek
	"
		'2008-W52' asWeek.
		'2008-W53' asWeek. 'Invalid format!!'.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
		'2008-W52' asWeek start.
		'2009-W01' asWeek start
		'2009-W02' asWeek start
		'2009-W53' asWeek start
		'2010-W01' asWeek start
		'2010-W02' asWeek start
	"

	^ Week fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:27:09'!
asYear
	"
		'2008' asYear.
		'2008' asYear start.
	"

	^ Year fromString: self! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:27:21'!
capitalized
	"Answer an object like the receiver but with first character downshifted if necesary
	'mElViN' capitalized
	'MElViN' capitalized
	('will', Random next mantissaPart printString) asSymbol capitalized
	('Will', Random next mantissaPart printString) asSymbol capitalized
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

!CharacterSequence methodsFor: 'converting' stamp: 'di 1/16/98 16:33'!
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
	'A clear but rather long-winded summary' contractTo: 18
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:28:57'!
findPositiveInteger 
	"Answer the Integer created by interpreting the receiver as the string representation of an integer.
	Answer nil if no digits, else find the first digit and then all consecutive digits after that"

	| startPosition tail endPosition |
	startPosition _ self findFirst: [:ch | ch isDigit].
	startPosition = 0 ifTrue: [^ nil].
	tail _ self copyFrom: startPosition to: self size.
	endPosition _ tail findFirst: [:ch | ch isDigit not].
	endPosition = 0 ifTrue: [endPosition _ tail size + 1].
	^ Number readFrom: (tail copyFrom: 1 to: endPosition - 1) readStream

"
'1796exportFixes-tkMX' findPositiveInteger
'1848recentLogFile-sw' findPositiveInteger
'donald' findPositiveInteger
'abc234def567' findPositiveInteger
"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:29:09'!
fixLineEndings
	"Assume that CR, LF, and CRLF are all valid line endings. Replace each occurence with a single Lf.
	If possible, modify and answer the same instance. Create a new instance only if needed.
	Be quick and don't use extra memory if already using Character lf!!
	('Hello ', String crlfString, 'World') fixLineEndings
	('Hello ', String crString, 'World') fixLineEndings
	('Hello ', String lfString, 'World') fixLineEndings
	"
	(self includes: `Character cr`) ifFalse: [ ^self ].
	^ self withLineEndings: `String newLineString`! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:29:33'!
initialIntegerOrNil
	"Answer the integer represented by the leading digits of the receiver, or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self isEmpty or: [self first isDigit not]) ifTrue: [^ nil].
	firstNonDigit _ (self findFirst: [:m | m isDigit not]).
	firstNonDigit = 0 ifTrue: [firstNonDigit _ self size + 1].
	^ (self copyFrom: 1  to: (firstNonDigit - 1)) asNumber
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"
! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:29:48'!
prefixAndSuffix: aCharacter
	"Answer an array with the prefix up to the last occurrence of aCharacter, and the suffix after it.
	Answer nil if aCharacter is not in self
	'1164-ReferenceStreamFix-jmv.1.cs' prefixAndSuffix: $-
	"
	| i |
	i _ self findLast: [ :c | c = aCharacter ].
	i = 0 ifTrue: [ ^ nil ].
	^ { self copyFrom: 1 to: i-1 . self copyFrom: i+1 to: self size }! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:30:02'!
squeezedTo: n
	"
Examples:
	Do nothing:
		'This one is a rather long phrase' squeezedTo: 32

	1-remove blanks (result can be shorter than asked):
		'This one is a rather long phrase' squeezedTo: 30

	2-remove necessary trailing vowels
		'This one is a rather long phrase' squeezedTo: 24

	3-truncate as needed (and add ellipsis)
		'This one is a rather long phrase' squeezedTo: 15

	4-avoid ellipsis
		'This one is a rather long phrase' squeezedTo: 5
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

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:30:22'!
uncapitalized
	"Answer an object like the receiver but with first character downshifted if necesary
	'MElViN' uncapitalized
	'mElViN' uncapitalized
	('Will', Random next mantissaPart printString) asSymbol uncapitalized
	('will', Random next mantissaPart printString) asSymbol uncapitalized
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

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:30:50'!
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
	' abc  d
	s
	as   zz 	q 			q' withBlanksCondensed
	"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:30:57'!
withCuisLineEndings
	"Assume that CR, LF, and CRLF are all valid line endings.
	Answer a new instance, replacing each occurence with a single Lf.
	('aLine', String crlfString, 'anotherOne') withCuisLineEndings
	"
	^ self withLineEndings: `String newLineString`.! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:31:03'!
withDescriptiveLineEndings
	"
	('Hello', String newLineString, 'World') withDescriptiveLineEndings
	"
	^ self withLineEndings: '[enter]'! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:31:12'!
withoutEnclosing: aCharacter
	"
	'*Hello*' withoutEnclosing: $*
	"
	| s |
	s _ self size.
	s = 0 ifTrue: [ ^ self ].
	^((self at: 1) = aCharacter and: [ (self at: s) = aCharacter ])
		ifTrue: [ self copyFrom: 2 to: s-1 ]
		ifFalse: [ self ]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:31:38'!
withoutPrefix: aString
	"
	'Elvis' withoutPrefix: 'El'
	'Elvis' withoutPrefix: 'Paul'
	"
	^(self beginsWith: aString)
		ifTrue: [ self copyFrom: aString size+1 to: self size ]! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:31:43'!
withoutSeparators
	
	^self reject: [ :aCharacter | aCharacter isSeparator ]

	"
	' abc  d   ' withoutSeparators
	"! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:31:47'!
withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last _ self lastNonSeparator.
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	"
	' abc  d   ' withoutTrailingBlanks
	"
! !

!CharacterSequence methodsFor: 'displaying' stamp: 'jmv 12/7/2022 09:28:40'!
displayProgressAt: aPoint from: minVal to: maxVal during: workBlock
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)

'Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 100
	during: [:barBlock |
		1 to: 100 do: [:x | 
			(Delay forMilliseconds: 10) wait.
			barBlock value: x.]].
	
['Now here''s some Real Progress'
	displayProgressAt: Sensor mousePoint
	from: 0 to: 10
	during: [:barBlock |
	1 to: 30 do: [:x | barBlock value: x \\ 11.
			(Delay forMilliseconds: 100) wait]]] fork

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.

The use of ProgressInitiationException allows for avoiding actual
progress display, by catching the exception.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock! !

!CharacterSequence methodsFor: 'formatting' stamp: 'jmv 12/7/2022 09:29:18'!
format: aCollection
	"format the receiver with aCollection  
	 
	simplest example:
	'foo {1} bar' format: {Date today}.
	 
	complete example:
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}..
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


!String methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:34:40'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(subString isUnicodeString and: [subString isAscii]) ifTrue: [
		^ String findString: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^ self asUnicodeString findString: subString startingAt: start ].
	"Quick primitive if both String"
	^ String findString: subString in: self startingAt: start! !

!String methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:34:43'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(subString isUnicodeString and: [ subString isAscii ]) ifTrue: [
		^String findStringIgnoreCase: subString bytes in: self startingAt: start ].
	"Do conversions if needed"
	subString isByteString ifFalse: [
		^self asUnicodeString findStringCaseInsensitive: subString startingAt: start ].
	"Quick primitive if both String"
	^String findStringIgnoreCase: subString in: self startingAt: start! !

!String methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:07:39'!
= aString
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, UnicodeString, UnicodeSymbol."

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

!String methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:34:37'!
caseInsensitiveLessOrEqual: aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case insensitive."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(aString isUnicodeString and: [ aString isAscii ]) ifTrue: [
		^(String compareIgnoreCase: self with: aString bytes) <= 2 ].
	"Do conversions if needed"
	aString isByteString ifFalse: [
		^self asUnicodeString caseInsensitiveLessOrEqual: aString ].
	"Quick primitive if both String"
	^ (String compareIgnoreCase: self with: aString) <= 2! !

!String methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:07:58'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	self size >  ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: String hash ].
	^ByteArray
		hashBytes: self
		startingWith: String hash! !

!String methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:16:38'!
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
	(subString isUnicodeString and: [ subString isAscii ]) ifTrue: [
		"Can use #bytes, because it has just ASCII bytes"
		subStringBytes _ subString bytes ].
	subStringBytes ifNotNil: [
		^(String findString: subStringBytes in: self startingAt: index) = index ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to String"
	^super is: subString substringAt: index! !

!String methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:34:51'!
sameAs: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"An ASCII UnicodeString is has same bytes as its String equivalent"
	(aString isUnicodeString and: [aString isAscii]) ifTrue: [
		^ (String compareIgnoreCase: self with: aString bytes) = 2 ].

	"Let them handle us"
	aString isByteString ifFalse: [ ^aString sameAs: self ].

	^ (String compareIgnoreCase: self with: aString) = 2! !

!String methodsFor: 'testing' stamp: 'jmv 12/7/2022 09:34:32'!
canHoldCodePoints
	"Nope. Needs something like UnicodeString for that. Even an Array is better than us."
	^false! !


!String class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:34:48'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ self new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each].
	^ newArray

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: UnicodeString
"! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:16:58'!
findString: subString startingAt: start
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| arg startByteIndex byteIndex |
	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findString: subString in: bytes startingAt: start ].
		subString isUnicodeString ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findString: subString bytes in: bytes startingAt: start ]]].

	arg _ subString asUtf8BytesOrByteString.
	"Do UTF-8 encoding as needed."
	startByteIndex _ self byteIndexAt: start.
	byteIndex _ String findString: arg in: bytes startingAt: startByteIndex.
	^self codePointIndexAt: byteIndex! !

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:17:00'!
findStringCaseInsensitive: subString startingAt: start
	"Answer the index in this String at which the substring key first occurs, at or beyond start.
	If no match is found, zero will be returned."

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		subString isByteString ifTrue: [
			^String findStringIgnoreCase: subString in: bytes startingAt: start ].
		subString isUnicodeString ifTrue: [
			^subString isAscii
				ifFalse: [ 0 ] "Won't be found, incudes non ASCII stuff, but we're ASCII"
				ifTrue: [ String findStringIgnoreCase: subString bytes in: bytes startingAt: start ]]].

"caseSensitive = false is very expensive. Do better."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase findString: subString asLowercase startingAt: start! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:35:15'!
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
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 1! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:35:19'!
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
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) <= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) <= 2! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:20:16'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, UnicodeString, UnicodeSymbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self hash = aString hash ifFalse: [
		^false ].

	(self isInCannonicalCompositionForm and: [ aString isInCannonicalCompositionForm ]) ifFalse: [
		^self cannonicalComposition = aString cannonicalComposition ].

	"Now we can assume we are both in NFC"
	self size = aString size ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [	
		"Fast lane."
		^String is: bytes equalTo: aString ].	

	aString isUnicodeString ifFalse: [
		^ self beginsWith: aString ].

	self isAscii = aString isAscii ifFalse: [
		^false ].

	^String is: bytes equalTo: aString bytes.! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:35:22'!
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
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 3]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 3! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:35:24'!
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
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) >= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character and UnicodeCodePoint"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) >= 2! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:35:58'!
basicHash
	"Same hash value as a pure ASCII String.
	Some other reasonable hash value otherwise, but not the same as the one of the equivalent String if ISO-8859 non ASCII characters are present.
	Note: will answer different values for UnicodeString's  that are equal if #cannonicalDecomposition is taken."
	bytes size >  ByteArray minSizeForHashQuick ifTrue: [
		^ bytes hashQuick: String hash ].
	^ByteArray
		hashBytes: bytes
		startingWith: String hash! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:16:55'!
caseInsensitiveLessOrEqual: aString
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"Fast lane. Use String implementation for ASCII only instances."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			^(String compareIgnoreCase: bytes with: aString) <= 2 ].
		(aString isUnicodeString and: [ aString isAscii ]) ifTrue: [
			^(String compareIgnoreCase: bytes with: aString bytes) <= 2 ]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase <= aString asLowercase! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:08:14'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, UnicodeString, UnicodeSymbol and Text
	if they hold same Characters."

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCannonicalCompositionAndHash ].

	"If 'hash' is not a number, it actually contains out #cannonicalComposition. Ask for its hash then."
	hash isNumber ifFalse: [
		^hash hash ].

	"If 'hash' is a number it means we are in cannonicalComposition form (NFC), and 'hash' is an good hash value."
	^hash! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:37:42'!
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
	subString isUnicodeString ifTrue: [
		"Simple. Take the bytes"
		subStringBytes _ subString bytes ].
	(subString isByteString and: [ subStringIsAscii ]) ifTrue: [
		"Can use instance of String, because it has just ASCII bytes"
		subStringBytes _ subString ].
	subStringBytes ifNotNil: [
		byteIndex _ self byteIndexAt: index.
		^(String findString: subStringBytes in: bytes startingAt: byteIndex) = byteIndex ].

	"Do it in Smalltalk. Most likely cheaper than converting argument to UnicodeString"
	^super is: subString substringAt: index! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 12/7/2022 09:17:06'!
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
		aString isUnicodeString ifTrue: [
			^ aString isAscii
				ifFalse: [ false ] "One is ASCII, other isn't."
				ifTrue: [
					"Both are ASCII"
					(String compareIgnoreCase: bytes with: aString bytes) = 2 ]]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT)."
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^self asLowercase = aString asLowercase! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:31:57'!
reversed
	"
	'frog' reversed
	'tomá agüita, ñandú' reversed
	"
	^UnicodeString fromCodePoints: self asCodePoints reversed! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:32:23'!
substrings
	"
	'  Hello World!! Some  		1234 more  ' substrings
	"
	^ self isAscii
		ifTrue: [ (String substringsIn: bytes) collect: [ :eachBytes |
				UnicodeString fromUtf8Bytes: eachBytes codePointCount: eachBytes size ]]
		ifFalse: [ (String substringsIn: bytes) collect: [ :eachBytes |
				UnicodeString fromUtf8Bytes: eachBytes ]].! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:38:30'!
surroundedBySingleQuotes
	"Answer the receiver with leading and trailing quotes.  "

	^ `(UnicodeString with: $')`, self, `(UnicodeString with: $')`! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:38:40'!
thatCanBeModified
	"Because UnicodeString instances don't support #at:put:"

	^ArrayOfCharactersAndCodePoints withAll: self! !

!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:40:12'!
withLineEndings: lineEndingString
	"assume the string is textual, and that CR, LF, and CRLF are all valid line endings. 
	Answer a new String, where each occurence with is replaced with lineEndingString"

	| correctedBytes |
	correctedBytes _ String string: bytes withLineEndings: lineEndingString asByteArray.
	"The codePointCount trick can be done because the line endings considered are all single byte UTF-8.
	If we ever consider unicode line endings taking more than one byte, this is no longer valid (pass nil instead)."
	^ UnicodeString fromUtf8Bytes: correctedBytes codePointCount: self size - bytes size + correctedBytes size.! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 09:35:36'!
basicCannonicalComposition
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	See examples at #cannonicalComposition.
	(UnicodeString fromCodePoints: #(119128 119141)) basicCannonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119135 119151)) basicCannonicalComposition asCodePoints
	(UnicodeString fromCodePoints: #(119128 119141 119151)) basicCannonicalComposition asCodePoints
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

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 09:35:49'!
basicCannonicallyOrdered
	"
	Usually prefer #cannonicalComposition or #cannonicalDecomposition
	https://unicode.org/reports/tr15/#Norm_Forms

	(UnicodeString fromCodePoints: #(16r61  16r0300 16r0315 16r031B)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0300 16r031B 16r0315)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r031B 16r0300)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r0315 16r0300 16r031B)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0300 16r0315)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromCodePoints: #(16r61  16r031B 16r0315 16r0300)) basicCannonicallyOrdered asCodePoints asArray collect: [ :e | e hex ]
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

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 09:36:10'!
cannonicalComposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFC form 'Normalization Form Composed'.

	The cannonical composition is cached. Therefor it doesn't need to be computed every time it is needed.
	This makes it actually cheaper than #cannonicalDecomposition!!

	UnicodeString fromUtf8Bytes: #[195 162].
	(UnicodeString fromUtf8Bytes: #[195 162]) asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) cannonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[195 162]) cannonicalComposition asUtf8Bytes

	UnicodeString fromUtf8Bytes: #[97 204 130].
	(UnicodeString fromUtf8Bytes: #[97 204 130]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[97 204 130]) cannonicalDecomposition asUtf8Bytes
	(UnicodeString fromUtf8Bytes: #[97 204 130]) cannonicalComposition asUtf8Bytes
	"

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCannonicalCompositionAndHash ].

	^hash isNumber
		ifTrue: [
			"If 'hash' is a number it means we are already in cannonicalComposition form (NFC)."
			self ]
		ifFalse: [
			"If 'hash' is not a number, it actually contains out #cannonicalComposition."
			hash ].! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 12/7/2022 09:36:27'!
cannonicalDecomposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(UnicodeCodePoint codePoint: 16r1EBF) asString
	(UnicodeCodePoint codePoint: 16r1EBF) asString asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCannonicalDecomposition asUtf8Bytes
	(UnicodeCodePoint codePoint: 16r1EBF) asString asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeCodePoint codePoint: 16r1EBF) asString basicCannonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]).
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCannonicalDecomposition asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCannonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeCodePoint codePoint: 119137) asString basicCannonicalDecomposition asCodePoints
	(UnicodeCodePoint codePoint: 119135) asString basicCannonicalDecomposition asCodePoints
	"
	^self basicCannonicalDecomposition basicCannonicallyOrdered! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:36:38'!
crString
	"Answer a string containing a carriage return.
	UnicodeString crString
	"
	^ `UnicodeString with: Character cr`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:36:44'!
crlfString
	"Answer a string containing a carriage return and a linefeed.
	UnicodeString crlfString
	"
	^ `UnicodeString with: Character cr with: Character lf`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:37:38'!
empty
	"Return the empty UnicodeString instance.
	Since my instances are readonly, this could be cached."

	^ self fromUtf8Bytes: `#[]` codePointCount: 0.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:41:13'!
lfString
	"Answer a string containing a single Lf character.
	UnicodeString lfString
	"
	^ `UnicodeString with: Character lf`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:37:50'!
newFrom: aSimilarObject
	^UnicodeString streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: UnicodeString
"! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:37:59'!
newLineString
	"Answer a string containing a single newLine (i.e. Lf) character.
	UnicodeString newLineString
	"
	^ `UnicodeString with: Character newLineCharacter`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:38:35'!
tab
	"Answer a string containing a single tab character.
	UnicodeString tab
	"
	^ `UnicodeString with: Character tab`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:38:51'!
with: aCharacter 
	"Answer a new instance of me, containing utf bytes for aCharacter.
	UnicodeString with: $a
	UnicodeString with: $á
	UnicodeString with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint)
		codePointCount: 1.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:38:56'!
with: aCharacter with: otherCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter and otherCharacter.
	UnicodeString with: $a
	UnicodeString with: $á with: Character euro
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint),
			(Character utf8BytesOfUnicodeCodePoint: otherCharacter codePoint)
		codePointCount: 2.! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:39:06'!
with: aCharacter with: otherCharacter with: thirdCharacter
	"Answer a new instance of me, containing utf bytes for aCharacter, otherCharacter and thirdCharacter.
	UnicodeString with: $a
	UnicodeString with: $á with: Character euro
	UnicodeString with: $á with: Character euro with: $¿
	"
	^self
		fromUtf8Bytes: (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint),
			(Character utf8BytesOfUnicodeCodePoint: otherCharacter codePoint),
			(Character utf8BytesOfUnicodeCodePoint: thirdCharacter codePoint)
		codePointCount: 3.! !

!UnicodeString class methodsFor: 'stream creation' stamp: 'jmv 12/7/2022 09:42:22'!
streamContents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, UnicodeStrings.
	Answer is an instance of us."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream contents! !

!UnicodeString class methodsFor: 'stream creation' stamp: 'jmv 12/7/2022 09:39:09'!
writeStream
	"Answer a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, UnicodeStrings."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	^stream! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:36:49'!
demoAncientEgyptian
	"
	UnicodeString demoAncientEgyptian.
	"
	| utf8 fontFamily |
	utf8 _  #[240 147 128 128 240 147 133 184 240 147 137 128 240 147 144 174 240 147 128 144 240 147 128 145 240 147 128 146 32 240 147 131 176 240 147 131 177 240 147 131 178 240 147 131 179 240 147 131 180 240 147 131 181 240 147 131 182 32 240 147 130 128 32 240 147 133 184 240 147 133 185 240 147 133 186 240 147 133 187 32 240 147 133 189 32 240 147 133 190 240 147 133 191 32 240 147 137 160 240 147 137 161 240 147 137 162 240 147 137 163 240 147 137 164 32 240 147 134 178 240 147 129 183 240 147 143 164 240 147 142 159 240 147 128 128 240 147 129 144 240 147 143 165 240 147 131 128 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 133 177 240 147 128 148 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 135 139 240 147 135 139 240 147 143 143 240 147 129 144 240 147 144 141 240 147 130 139 240 147 139 180 240 147 130 157 240 147 142 155 240 147 139 169 240 147 137 148 240 147 138 170 240 147 143 155 240 147 139 180 240 147 144 160 240 147 132 191 240 147 130 139 240 147 143 143 240 147 140 151 240 147 128 129 240 147 140 183 240 147 130 157 240 147 143 143 240 147 143 173 240 147 143 155 240 147 135 190 240 147 143 143 240 147 133 147 240 147 133 177 240 147 128 128 240 147 129 144 240 147 143 170 240 147 131 128 240 147 140 162 240 147 140 162 240 147 136 150 240 147 136 150 240 147 143 155].

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'NotoEgyptianHieroglyphs'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 60))
		editLabel: 'Ancient Egyptian Hieroglyphs!!'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:36:55'!
demoArabic
	"
	UnicodeString demoArabic
	"
	| utf8 |
	utf8 _  #[217 131 217 132 32 216 163 217 136 216 179 216 185 32 216 167 217 132 216 170 217 134 216 167 216 178 217 132 217 138 32 217 129 216 181 217 132 44 32 217 133 216 185 32 216 185 217 133 217 132 217 138 216 169 32 216 167 217 132 216 180 216 177 217 130 217 138 32 216 167 217 134 216 170 216 168 216 167 217 135 32 217 138 217 131 217 134 44 32 216 185 217 132 32 216 168 216 173 217 130 32 216 171 217 133 217 145 216 169 32 217 136 216 173 217 132 217 129 216 167 216 164 217 135 216 167 46 32 217 133 216 185 32 216 182 216 177 216 168 32 216 175 217 129 217 145 216 169 32 216 180 216 177 216 179 216 169 32 216 167 217 132 216 181 217 129 216 173 216 167 216 170 44 32 217 133 216 179 216 164 217 136 217 132 217 138 216 169 32 216 167 217 132 216 177 216 168 217 138 216 185 216 140 32 217 132 216 168 217 136 217 132 217 134 216 175 216 167 216 140 32 217 136 216 170 217 133 32 217 130 216 175 46 32 216 174 216 183 217 145 216 169 32 217 132 217 133 216 173 216 167 217 131 217 133 32 217 136 216 181 217 132 32 217 129 217 138 44 32 217 136 217 129 217 134 217 132 217 134 216 175 216 167 32 216 167 217 132 216 165 217 134 216 178 216 167 217 132 32 216 167 217 132 217 133 216 170 216 167 216 174 217 133 216 169 32 216 185 217 134 32 216 173 217 138 216 171 46 32 216 167 217 132 217 137 32 217 135 216 167 217 133 216 180 32 216 167 217 132 217 138 216 167 216 168 216 167 217 134 32 217 132 217 133 46 32 216 170 216 183 217 136 217 138 216 177 32 217 129 217 135 216 177 216 179 216 170 32 217 132 217 133 32 217 129 216 185 217 132 46 32 216 175 217 134 217 136 32 216 167 217 132 216 185 216 179 217 131 216 177 217 138 32 217 136 217 133 216 183 216 167 217 132 216 168 216 169 32 216 168 217 132 44 32 217 133 217 132 217 138 217 136 217 134 32 217 136 216 168 216 167 216 179 216 170 216 171 217 134 216 167 216 161 32 217 133 216 185 32 216 175 217 136 217 134 44 32 217 133 217 134 32 217 133 217 131 217 134 32 216 165 216 168 217 145 216 167 217 134 32 216 167 217 132 216 172 217 134 217 136 216 168 217 138 46 32 216 168 217 128 32 217 133 216 167 216 176 216 167 32 216 163 217 136 216 177 216 167 217 130 217 135 217 133 32 216 173 217 138 216 171 44 32 217 131 216 167 217 134 32 216 170 217 133 32 216 179 216 167 216 168 217 130 32 217 133 217 131 217 145 217 134 32 216 167 217 132 216 179 217 138 216 161 44 32 216 163 216 179 216 177 32 217 129 217 135 216 177 216 179 216 170 32 216 167 217 132 216 179 216 167 216 175 216 179 32 216 167 217 132 216 165 216 173 216 170 217 129 216 167 216 184 32 217 136 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24)) editLabel: 'Arabic'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:37:00'!
demoArmenian
	"
	UnicodeString demoArmenian
	"
	| utf8 |
	utf8 _  #[213 172 213 184 213 188 213 165 213 180 32 213 171 213 186 213 189 213 184 214 130 213 180 32 213 164 213 184 213 172 213 184 213 188 32 213 189 213 171 213 169 32 213 161 213 180 213 165 213 169 44 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 165 213 184 214 130 213 171 213 189 213 180 213 184 213 164 32 213 188 213 165 213 186 213 184 214 130 213 164 213 171 213 161 213 188 213 165 32 213 184 214 130 213 169 32 213 189 213 171 213 169 46 32 213 190 213 165 213 188 213 184 32 213 188 213 165 214 134 213 184 213 188 213 180 213 171 213 164 213 161 213 182 213 189 32 213 174 213 184 214 130 32 213 189 213 171 213 169 44 32 213 164 213 165 213 162 213 165 213 169 32 213 186 213 165 213 188 214 134 213 165 213 174 213 169 213 184 32 213 189 213 161 213 172 213 184 214 130 213 169 213 161 213 169 213 184 214 130 213 189 32 213 165 213 169 32 213 174 213 184 214 130 213 180 44 32 213 176 213 161 213 189 32 213 169 213 188 213 171 213 169 213 161 213 182 213 171 32 213 182 213 184 213 182 213 184 214 130 213 180 213 165 213 189 32 213 182 213 165 46 32 213 180 213 165 213 171 32 213 182 213 184 32 213 189 213 184 213 172 213 184 214 130 213 169 213 161 32 213 172 213 161 213 162 213 184 213 188 213 165 213 189 32 213 184 213 180 213 171 213 169 213 169 213 161 213 180 44 32 213 165 213 173 32 213 163 213 188 213 161 213 165 213 174 213 184 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 189 32 213 176 213 161 213 189 44 32 213 165 213 161 32 213 180 213 165 213 172 32 214 132 213 184 214 130 213 171 213 189 32 213 182 213 171 213 189 213 172 32 213 161 213 189 213 189 213 184 214 130 213 180 46 32 213 171 213 182 32 213 161 213 180 213 165 213 169 32 213 182 213 165 213 180 213 184 213 188 213 165 32 213 184 214 130 213 189 213 184 214 130 46 32 213 165 213 188 213 161 213 169 32 213 171 213 184 214 130 213 190 213 161 213 188 213 165 213 169 32 213 165 213 161 32 213 189 213 165 213 164 44 32 213 176 213 161 213 189 32 213 161 213 172 213 171 213 161 32 214 132 213 184 214 130 213 161 213 165 214 132 213 184 214 130 213 165 32 213 174 213 184 213 188 213 186 213 184 213 188 213 161 32 213 161 213 164 46 32 213 165 213 184 213 189 32 214 134 213 161 213 189 213 169 213 171 213 164 213 171 213 171 32 213 164 213 165 214 134 213 171 213 182 213 171 213 169 213 171 213 184 213 182 213 165 213 180 32 213 182 213 165 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24)) 
		editLabel: 'Armenian'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:37:06'!
demoChinese
	"
	UnicodeString demoChinese
	"
	| utf8 fontFamily |
	utf8 _  #[233 159 179 229 138 155 233 164 168 229 136 134 233 167 191 229 155 163 230 152 147 229 142 154 229 165 189 230 149 151 231 132 188 233 150 139 229 186 183 229 134 134 227 128 130 230 149 133 229 176 143 229 162 151 233 157 162 230 150 135 230 169 139 232 187 162 229 149 143 233 150 128 228 188 154 231 142 132 233 150 139 229 142 187 230 137 128 230 173 163 230 131 179 232 169 177 230 157 165 227 128 130 229 185 180 228 186 161 229 166 187 229 173 152 232 186 171 230 143 144 233 128 159 228 186 172 229 186 171 230 153 130 228 184 188 230 177 186 232 131 189 227 128 130 229 143 184 229 174 159 229 134 133 229 177 164 232 179 170 229 143 163 232 178 162 230 177 130 229 133 172 233 157 153 230 179 129 229 136 182 229 188 149 230 153 130 229 157 135 233 171 152 230 150 176 227 128 130 231 162 186 230 156 159 230 149 176 233 155 134 230 156 168 232 172 155 230 173 162 230 178 191 229 130 153 233 129 142 229 155 189 232 179 188 230 168 170 229 150 156 229 136 183 229 186 183 230 136 144 229 175 186 227 128 130 232 168 152 229 136 134 229 143 130 229 145 188 229 179 182 229 155 189 232 170 160 230 168 169 229 155 158 230 173 163 229 185 180 232 187 141 230 167 152 228 187 138 229 132 170 230 150 135 231 166 129 229 144 166 227 128 130 230 149 180 230 130 163 232 182 163 231 164 190 230 152 142 230 164 156 231 149 170 230 158 156 230 142 168 231 178 149 228 184 150 230 156 172 229 176 143 231 132 161 231 167 176 231 148 186 227 128 130 230 138 149 230 160 188 229 140 187 229 190 140 228 187 187 231 179 187 232 187 162 230 148 191 230 157 161 232 169 177 230 142 178 230 156 136 231 144 131 227 128 130 230 130 148 233 133 184 232 166 139 229 164 169 229 134 153 229 164 137 229 143 130 233 153 184 229 155 155 229 144 141 229 140 150 232 139 151 229 141 136 230 178 187 233 154 155 228 189 143 232 166 135 228 184 184 230 149 153 231 149 140].

	fontFamily _ FontFamily readIfNeeded: DirectoryEntry smalltalkImageDirectory / 'TrueTypeFonts'/ 'KiwiMaru'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize familyName: fontFamily familyName pointSize: 24))
		editLabel: 'Chinese'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:37:14'!
demoCyrillic
	"
	UnicodeString demoCyrillic
	"
	| utf8 |
	utf8 _  #[208 155 208 190 209 128 208 181 208 188 32 208 184 208 191 209 129 209 131 208 188 32 208 180 208 190 208 187 208 190 209 128 32 209 129 208 184 209 130 32 208 176 208 188 208 181 209 130 44 32 208 180 208 181 208 177 208 181 209 130 32 209 129 208 176 208 187 209 131 209 130 208 176 208 189 208 180 208 184 32 208 188 208 181 208 184 32 208 181 208 184 46 32 208 144 208 189 32 209 133 208 184 209 129 32 208 179 209 128 208 176 208 181 209 134 208 181 32 209 131 209 130 208 176 208 188 209 131 209 128 32 208 180 208 190 208 187 208 190 209 128 209 131 208 188 44 32 208 187 208 190 209 128 208 181 208 188 32 208 191 209 128 208 184 208 189 209 134 208 184 208 191 208 181 209 129 32 208 188 208 190 208 187 208 181 209 129 209 130 208 184 208 176 208 181 32 208 180 209 131 208 190 32 208 184 208 189 46 32 208 149 208 184 32 208 188 208 190 208 178 208 181 209 130 32 208 189 208 190 209 129 209 130 209 128 208 190 32 208 190 208 191 209 130 208 184 208 190 208 189 32 208 189 208 176 208 188 44 32 208 181 208 190 209 129 32 208 181 208 191 208 184 209 134 209 131 209 128 208 184 32 208 190 208 188 208 184 209 130 209 130 208 176 208 189 209 130 209 131 209 128 32 209 131 209 130 46 32 208 163 209 130 32 208 188 209 131 208 189 208 181 209 128 208 181 32 208 187 208 176 208 177 208 190 209 128 208 181 32 208 184 208 189 209 130 208 181 208 187 208 187 208 181 208 179 208 181 208 177 208 176 209 130 32 209 131 209 129 209 131 44 32 208 181 208 176 32 209 129 208 181 208 180 32 208 180 208 181 208 177 208 181 209 130 32 208 187 209 131 208 191 209 130 208 176 209 130 209 131 208 188 44 32 208 178 208 184 208 180 208 184 209 130 32 208 184 208 187 208 187 209 131 208 180 32 208 184 209 131 208 180 208 184 209 134 208 190 32 209 143 209 131 208 190 32 209 131 209 130 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Cyrillic'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:37:19'!
demoGeorgian
	"
	UnicodeString demoGeorgian
	"
	| utf8 |
	utf8 _  #[225 131 154 225 131 157 225 131 160 225 131 148 225 131 155 32 225 131 152 225 131 164 225 131 161 225 131 163 225 131 155 32 225 131 147 225 131 157 225 131 154 225 131 157 225 131 160 32 225 131 161 225 131 152 225 131 151 32 225 131 144 225 131 155 225 131 148 225 131 151 44 32 225 131 167 225 131 163 225 131 157 32 225 131 163 225 131 151 32 225 131 147 225 131 152 225 131 170 225 131 144 225 131 151 32 225 131 144 225 131 154 225 131 152 225 131 167 225 131 163 225 131 152 225 131 147 44 32 225 131 144 225 131 147 32 225 131 154 225 131 152 225 131 145 225 131 160 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 160 225 131 148 225 131 164 225 131 160 225 131 148 225 131 176 225 131 148 225 131 156 225 131 147 225 131 163 225 131 156 225 131 151 32 225 131 148 225 131 161 225 131 151 46 32 225 131 154 225 131 148 225 131 146 225 131 148 225 131 160 225 131 148 32 225 131 170 225 131 157 225 131 160 225 131 164 225 131 157 225 131 160 225 131 144 32 225 131 148 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 144 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 144 225 131 151 32 225 131 148 225 131 144 225 131 155 44 32 225 131 156 225 131 157 32 225 131 167 225 131 163 225 131 144 225 131 161 32 225 131 155 225 131 148 225 131 156 225 131 144 225 131 156 225 131 147 225 131 160 225 131 152 32 225 131 160 225 131 148 225 131 164 225 131 148 225 131 160 225 131 160 225 131 148 225 131 156 225 131 151 225 131 163 225 131 160 32 225 131 167 225 131 163 225 131 152 46 32 225 131 151 225 131 148 32 225 131 164 225 131 163 225 131 151 225 131 148 225 131 156 225 131 151 32 225 131 155 225 131 157 225 131 154 225 131 154 225 131 152 225 131 161 32 225 131 170 225 131 148 225 131 151 225 131 148 225 131 160 225 131 157 225 131 161 32 225 131 149 225 131 152 225 131 161 46 32 225 131 149 225 131 152 225 131 155 32 225 131 147 225 131 148 225 131 156 225 131 152 225 131 167 225 131 163 225 131 148 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 151 225 131 176 225 131 148 225 131 157 225 131 164 225 131 176 225 131 160 225 131 144 225 131 161 225 131 151 225 131 163 225 131 161 32 225 131 148 225 131 174 44 32 225 131 148 225 131 144 32 225 131 176 225 131 144 225 131 161 32 225 131 149 225 131 157 225 131 154 225 131 163 225 131 155 225 131 163 225 131 161 32 225 131 164 225 131 144 225 131 161 225 131 151 225 131 152 225 131 147 225 131 152 225 131 152 32 225 131 147 225 131 148 225 131 161 225 131 148 225 131 160 225 131 163 225 131 156 225 131 151 46 32 225 131 148 225 131 174 32 225 131 157 225 131 164 225 131 164 225 131 152 225 131 170 225 131 152 225 131 152 225 131 161 32 225 131 151 225 131 157 225 131 160 225 131 167 225 131 163 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 155 225 131 148 225 131 152 44 32 225 131 147 225 131 157 225 131 170 225 131 151 225 131 163 225 131 161 32 225 131 149 225 131 152 225 131 149 225 131 148 225 131 156 225 131 147 225 131 163 225 131 155 32 225 131 152 225 131 156 32 225 131 167 225 131 163 225 131 157 46 32 225 131 161 225 131 163 225 131 155 225 131 155 225 131 157 32 225 131 151 225 131 160 225 131 144 225 131 170 225 131 151 225 131 144 225 131 151 225 131 157 225 131 161 32 225 131 148 225 131 152 32 225 131 152 225 131 163 225 131 161 44 32 225 131 163 225 131 151 32 225 131 157 225 131 155 225 131 156 225 131 148 225 131 161 32 225 131 149 225 131 152 225 131 160 225 131 151 225 131 163 225 131 151 225 131 148 32 225 131 147 225 131 148 225 131 151 225 131 160 225 131 144 225 131 174 225 131 152 225 131 151 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Georgian'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:37:23'!
demoGreek
	"
	UnicodeString demoGreek
	"
	| utf8 |
	utf8 _  #[206 155 206 191 207 129 206 181 206 188 32 206 185 207 128 207 131 206 184 206 188 32 206 180 206 191 206 187 206 191 207 129 32 207 131 206 185 207 132 32 206 177 206 188 206 181 207 132 44 32 206 188 206 191 206 180 206 191 32 118 206 181 206 189 206 185 206 177 206 188 32 118 206 185 206 180 206 181 207 129 206 181 207 129 32 206 185 206 189 32 99 206 184 206 188 44 32 206 181 206 190 32 206 177 206 184 207 132 206 181 206 188 32 206 188 206 177 206 187 206 185 207 131 32 206 191 207 134 207 134 206 181 206 189 206 180 206 185 207 132 32 206 181 207 131 207 132 46 32 206 153 206 180 32 207 132 206 191 206 187 206 187 206 185 207 132 32 207 131 206 184 207 131 99 206 185 207 128 206 185 207 132 32 207 131 206 177 206 187 206 184 207 132 206 177 207 132 206 184 207 131 32 113 206 184 206 185 44 32 99 206 184 32 113 206 184 206 177 206 189 206 180 206 191 32 207 128 207 129 206 191 207 128 207 129 206 185 206 177 206 181 32 206 185 206 188 207 128 206 181 207 129 206 180 206 185 206 181 207 132 32 118 206 181 206 187 44 32 206 188 206 181 206 177 32 206 181 207 132 32 206 181 207 129 207 129 206 181 206 188 32 206 179 206 187 206 191 207 129 206 185 206 177 207 132 206 184 207 129 46 32 206 149 206 190 206 181 207 129 99 206 185 32 206 180 206 181 207 134 206 185 206 189 206 185 207 132 206 185 206 191 206 189 206 181 206 188 32 206 188 206 181 206 187 32 206 181 206 177 46 32 206 149 206 190 32 207 131 206 181 206 177 32 206 181 206 187 206 185 207 132 32 206 180 206 185 207 131 207 131 206 181 206 189 207 132 206 185 206 181 207 132 44 32 206 188 206 177 206 187 206 185 207 131 32 207 132 207 129 206 185 207 132 206 177 206 189 206 185 32 118 206 185 207 131 32 206 185 206 180 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Greek'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:37:28'!
demoHebrew
	"
	UnicodeString demoHebrew
	"
	| utf8 |
	utf8 _  #[215 162 215 156 32 215 158 215 144 215 158 215 168 32 215 150 215 167 215 149 215 167 32 215 169 215 147 215 168 215 149 215 170 32 215 144 215 168 215 165 44 32 215 148 215 158 215 156 215 166 215 170 32 215 158 215 147 215 149 215 153 215 167 215 153 215 157 32 215 169 215 155 215 156 32 215 169 215 156 44 32 215 162 215 153 215 166 215 149 215 145 32 215 160 215 153 215 149 215 149 215 152 32 215 169 215 153 215 170 215 149 215 164 215 153 215 170 32 215 162 215 149 215 147 32 215 145 46 32 215 161 215 147 215 168 32 215 144 215 149 32 215 168 215 164 215 149 215 144 215 148 32 215 169 215 153 215 158 215 149 215 169 215 153 32 215 158 215 149 215 162 215 158 215 147 215 153 215 157 46 32 215 148 215 153 215 144 32 215 164 215 153 215 161 215 153 215 167 215 148 32 215 148 215 146 215 168 215 164 215 153 215 157 32 215 169 215 153 215 158 215 149 215 169 215 153 215 153 215 157 32 215 169 215 156 46 32 215 169 215 155 215 156 32 215 162 215 156 32 215 162 215 150 215 168 215 148 32 215 156 215 148 215 164 215 149 215 154 32 215 144 215 149 215 149 215 153 215 168 215 149 215 160 215 144 215 149 215 152 215 153 215 167 215 148 44 32 215 144 215 160 215 144 32 215 144 215 170 32 215 162 215 150 215 168 215 148 32 215 156 215 158 215 160 215 149 215 162 32 215 156 215 158 215 151 215 153 215 167 215 148 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Hebrew'! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/7/2022 09:37:33'!
demoLatin
	"
	UnicodeString demoLatin
	"
	| utf8 |
	utf8 _  #[76 111 114 101 109 32 105 112 115 117 109 32 100 111 108 111 114 32 115 105 116 32 97 109 101 116 44 32 112 111 112 117 108 111 32 108 97 116 105 110 101 32 97 108 105 113 117 105 100 32 112 114 111 32 110 111 44 32 115 101 100 32 116 101 32 103 114 97 101 99 111 32 112 101 114 99 105 112 105 116 32 97 100 105 112 105 115 99 105 110 103 46 32 68 101 102 105 110 105 101 98 97 115 32 100 105 115 115 101 110 116 105 97 115 32 109 101 97 32 105 110 44 32 118 101 108 32 110 117 108 108 97 32 116 111 108 108 105 116 32 101 120 46 32 65 108 105 113 117 105 112 32 112 101 114 115 101 113 117 101 114 105 115 32 101 108 111 113 117 101 110 116 105 97 109 32 104 105 115 32 101 116 46 32 67 117 32 100 105 97 109 32 110 111 118 117 109 32 97 110 105 109 97 108 32 118 105 120 44 32 110 97 109 32 101 116 32 112 114 111 98 97 116 117 115 32 116 114 97 99 116 97 116 111 115 32 100 101 116 101 114 114 117 105 115 115 101 116 46 32 80 101 114 99 105 112 105 116 32 101 102 102 105 99 105 97 110 116 117 114 32 112 114 105 32 101 120 44 32 105 100 32 97 110 105 109 97 108 32 100 101 98 105 116 105 115 32 118 117 108 112 117 116 97 116 101 32 101 111 115 44 32 97 100 32 105 117 115 32 112 111 115 115 101 32 101 114 114 111 114 32 112 111 112 117 108 111 46].

	(Text string: (UnicodeString fromUtf8Bytes: utf8) attribute: (TextFontFamilyAndSize pointSize: 24))
		editLabel: 'Latin Alphabet'! !


!UnicodeSymbol class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:20:39'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ (UnicodeString newFrom: aCollection) asSymbol

"
	UnicodeSymbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: UnicodeSymbol
"! !

!UnicodeSymbol class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:20:32'!
readFrom: strm
	"
	UnicodeSymbol readFrom: '#abc' readStream
	"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !


!ByteArray class methodsFor: 'stream creation' stamp: 'jmv 12/7/2022 09:39:12'!
streamUtf8Contents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, UnicodeStrings.
	Answer is an instance of us, holding UTF-8 encoded bytes."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream byteContents! !


!UniFileStream methodsFor: 'open/close' stamp: 'jmv 12/7/2022 09:18:33'!
open: fileName forWrite: writeMode
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ UniFileStream retryWithGC: [ self primOpen: fileName asUtf8Bytes writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ ByteArray new: 1.
	self useUnicodeString.
	self enableReadBuffering.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/7/2022 09:39:19'!
next
	"Answer the next entry from this file, or nil if at the end of the file.
	Answer will be
	- CodePoint (for #useUnicodeString)
	- Character (for #useCharacters)
	- SmallInteger (for #useBytes)"

	^readAdaptor next! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/7/2022 09:39:23'!
next: n
	"Answer the next n charaters (or bytes) from this file, or nil if at the end of the file.
	Answer will be
	- an UnicodeString or String or size n (or until the end of the file if shorter)
	- a ByteArray of size n (or until the end of the file if shorter)"

	^readAdaptor next: n! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/7/2022 09:39:27'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, UnicodeString or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: aStringOrBytes startingAt: startIndex count: anInteger.
	lastWrittenElement _ aStringOrBytes at: startIndex+anInteger-1.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/7/2022 09:39:29'!
nextPutAll: aStringOrBytes
	"Argument can be String, UnicodeString or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes].
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: bytes startingAt: 1 count: bytes basicSize.
	lastWrittenElement _ aStringOrBytes last.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 12/7/2022 09:39:36'!
useBytes
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor _ BytesReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 12/7/2022 09:39:41'!
useCharacters
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor _ ByteStringReadAdaptor new stream: self.! !


!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 12/7/2022 09:40:28'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, UnicodeString or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	super next: anInteger putAll: aStringOrBytes startingAt: startIndex.
	lastWrittenElement _ aStringOrBytes at: startIndex+anInteger-1.
	numberOfCodePointsWritten _ nil.	"We can no longer know"! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 12/7/2022 09:40:30'!
nextPutAll: aStringOrBytes
	"Argument can be String, UnicodeString or ByteArray.
	Write to this stream. Both kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes.
		numberOfCodePointsWritten ifNotNil: [
			"As aStringOrBytes is a kind of String, not utf-8 byteArray, we can use its size."
			numberOfCodePointsWritten _ numberOfCodePointsWritten + aStringOrBytes size ]].
	super nextPutAll: bytes.
	lastWrittenElement _ aStringOrBytes last.! !

!Utf8EncodedWriteStream methodsFor: 'accessing' stamp: 'jmv 12/7/2022 09:42:14'!
contents
	"Answer the UnicodeString built so far."

	^UnicodeString fromUtf8Bytes: self byteContents codePointCount: numberOfCodePointsWritten.! !


!UnicodeReadAdaptor methodsFor: 'reading' stamp: 'jmv 12/7/2022 09:40:45'!
next: n
	"Read n UnicodeCodePoints, or until the end of the file.
	Answer them, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Answer is an instance of UnicodeString."

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


!Transcript class methodsFor: 'displaying' stamp: 'jmv 12/7/2022 09:13:30'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	displayEngine colorMap: nil.
	^s asUnicodeString displayOnBitBltCanvasEngine: displayEngine
		from: 1
		to: s size
		at: pt roundedHAFZ
		font: font
		color: aColor! !


!CharacterScanner methodsFor: 'scanning' stamp: 'jmv 12/7/2022 09:40:57'!
scanCharactersFrom: startIndex to: stopIndex in: anUnicodeString rightX: rightX stopConditions: stops
	"This is the inner loop of text display.
	March through source String from startIndex to stopIndex. If any character is flagged with a
	non-nil entry in stops, then return the corresponding value.
	Determine width of each character.
	If dextX would exceed rightX, then return stops at: 258.
	Advance destX by the width of the character. If stopIndex has been
	reached, then return stops at: 257."

	| nextDestX |
	lastIndex _ startIndex.
	anUnicodeString from: startIndex to: stopIndex doWithUtf8Bytes: [ :byte1 :byte2 :byte3 :byte4 |
		"stops are only defined for the first 128 characters
		(entries 128 to 255 are always nil).
		It is OK to just use the first byte in UTF-8"
		(stops at: byte1 + 1) ifNotNil: [ :stop | ^stop ].
		nextDestX _ destX + (font widthOfUtf8Byte1: byte1 b2: byte2 b3: byte3 b4: byte4).
		nextDestX > rightX ifTrue: [
			^stops at: CharacterScanner crossedXCode ].
			destX _ nextDestX.
			lastIndex _ lastIndex + 1 ].
	lastIndex _ stopIndex.
	^ stops at: CharacterScanner endOfRunCode! !


!CharacterBlockScanner methodsFor: 'scanning' stamp: 'jmv 12/7/2022 09:13:33'!
characterBlockAtPoint: aPoint index: index in: textLine
	"This method is the Morphic characterBlock finder."
	| runLength lineStop stopCondition string |
	line _ textLine.
	rightMargin _ line rightMargin.
	lastIndex _ line first.
	tabCount _ 0.
	self setFont.
	self setStopConditions.
	characterIndex _ index.  "nil means scanning for point"
	characterPoint _ aPoint.
	(characterPoint isNil or: [ characterPoint y > line bottom ])
		ifTrue: [ characterPoint _ line bottomRight ].
	(text isEmpty or: [( characterPoint y < line top or: [ characterPoint x < line left ])
				or: [ characterIndex notNil and: [ characterIndex < line first ]]])
		ifTrue:	[^ CharacterBlock
					stringIndex: line first
					text: text
					topLeft: line leftMargin@line top
					extent: 0 @ line lineHeight
					textLine: line].
	 destX _ leftMargin _ line leftMarginForAlignment: alignment.
	destY _ line top.
	runLength _ text runLengthFor: line first.
	lineStop _ characterIndex	"scanning for index"
		ifNil: [ line last ].			"scanning for point"
	runStopIndex _ lastIndex + (runLength - 1) min: lineStop.
	lastCharacterWidth _ 0.
	spaceCount _ 0.
	string _ text string asUnicodeString.

	self placeEmbeddedObject.
	[
		stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: characterPoint x
			stopConditions: stopConditions.
		"see setStopConditions for stopping conditions for character block operations."
		lastCharacterWidth _ specialWidth ifNil: [ font widthOf: (text at: lastIndex) ].
		(self perform: stopCondition) ifTrue: [
			^characterIndex
				ifNil: [	"Result for characterBlockAtPoint: "
					CharacterBlock
						stringIndex: lastIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]
				ifNotNil: [	"Result for characterBlockForIndex: "
					CharacterBlock
						stringIndex: characterIndex
						text: text
						topLeft: characterPoint x@line top
						extent: lastCharacterWidth @ line lineHeight
						textLine: line ]]
		] repeat! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 12/7/2022 09:13:36'!
composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength stopCondition xtraSpaceBefore spaceAfterParagraph string |
	
	lastIndex _ startIndex.	"scanning sets last index"
	tabCount _ 0.
	destY _ lineRectangle top.
	lineHeight _ baseline _ 0.  "Will be increased by setFont"
	self setFont.
	self setStopConditions.

	"Set up margins"
	leftMargin _ lineRectangle left.
	rightMargin _ lineRectangle right.
	xtraSpaceBefore _ 0.
	spaceAfterParagraph _ 0.
	paragraphStyle ifNotNil: [
		leftSide ifTrue: [
			leftMargin _ leftMargin +
				((firstLine and: [ paragraphStyle isListStyle not ])
					ifTrue: [ paragraphStyle firstIndent ]
					ifFalse: [ paragraphStyle restIndent ])].
		rightSide ifTrue: [
			rightMargin _ rightMargin - paragraphStyle rightIndent].
		firstLine ifTrue: [ xtraSpaceBefore _ paragraphStyle spaceBefore ].
		spaceAfterParagraph _ paragraphStyle spaceAfter ].
	destX _ spaceX _ leftMargin.

	runLength _ text runLengthFor: startIndex.
	runStopIndex _ lastIndex + runLength - 1.
	line _ (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount _ 0.
	lastLineBreakingSpace _ 0.
	leftMargin _ destX.
	line leftMargin: leftMargin.
	string _ text string asUnicodeString.

	self placeEmbeddedObject.
	[
			stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
				in: string rightX: rightMargin stopConditions: stopConditions.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]
	] repeat! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 12/7/2022 09:13:39'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos x1 |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastIndex _ line first.
	tabCount _ 0.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor _ defaultColor.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				spaceWidth _ font widthOf: Character space.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]]].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string asUnicodeString.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions.
		backgroundColor ifNotNil: [
			x1 _ destX.
			((Preferences at: #backgroundColorFillsAllBackground) and: [startIndex > line last]) ifTrue: [
				x1 _ rightMargin ].
			canvas
				fillRectangle: (lastPos corner: x1 @ (line bottom + textTopLeft y))
				color: backgroundColor.
			((Preferences at: #backgroundColorFillsAllBackground) and: [stopCondition = #tab]) ifTrue: [
				canvas
					fillRectangle: (destX @ lastPos y corner: self tabDestX @ (line bottom + textTopLeft y))
					color: backgroundColor ]].
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat! !


!AbstractVectorCanvas methodsFor: 'drawing - text' stamp: 'jmv 12/7/2022 09:13:42'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	^ aString asUnicodeString displayOnVectorEngine: engine from: firstIndex to: lastIndex
		at: aPoint font: aTrueTypeFont color: aColor.! !


!VectorCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/7/2022 09:13:45'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor
	"Answer position to place next glyph"
	"
| s c f |
c _ VectorCanvas onForm: Display.
c geometryTransformation: (AffineTransformation withRadians: 0.1).
f _ FontFamily familyName: 'DejaVu Sans' pointSize: 72.
s _ 'Latin text in VectorGraphics'.
(c drawString: s from: 1 to: s size atBaseline: 100@100 font: f color: Color black) print.
c strokeWidth: 1 color: Color red do: [ c moveTo: 100@100; lineTo: 1000@100 ].
Display forceToScreen
	"
	| p1 answer |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	currentTransformation isPureTranslation ifTrue: [
		(Preferences at: #cacheTrueTypeGlyphs) ifTrue: [
			p1 _ currentTransformation transform: aPoint roundedHAFZ.
			p1 _ p1 + (0@(0 - (aTrueTypeFont ascent + aTrueTypeFont lineGap-1))).
			p1 _ p1 roundedHAFZ.
			answer _ aString asUnicodeString displayOnBitBltCanvasEngine: auxBitBltEngine
				from: firstIndex to: lastIndex at: p1 font: aTrueTypeFont color: aColor.
			answer ifNotNil: [
				engine updateMorphBoundsLeft: p1 x top: p1 y
					right: answer x bottom: answer y ].
			^answer ]].

	^super drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: aTrueTypeFont color: aColor! !


!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 12/7/2022 09:13:47'!
drawString: aString from: firstIndex to: lastIndex atBaseline: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	"Don't waste any time if NOP"
	lastIndex = 0 ifTrue: [
		^nil ].

	p1 _ currentTransformation transform: aPoint roundedHAFZ.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 roundedHAFZ.
	engine colorMap: nil.
	^aString asUnicodeString displayOnBitBltCanvasEngine: engine
		from: firstIndex
		to: lastIndex
		at: p1
		font: font
		color: aColor! !


!DifferenceFinder class methodsFor: 'compatibility' stamp: 'jmv 12/7/2022 09:14:00'!
wordsDisplayPatchFrom: srcStringOrText to: dstStringOrText

	| finder answer src1 dst1 changedCount dstString srcString |
	srcString _ srcStringOrText asPlainString.
	dstString _ dstStringOrText asPlainString.
	srcString class == dstString class ifFalse: [
		^self wordsDisplayPatchFrom: srcString asUnicodeString to: dstString asUnicodeString ].

	finder _ self base: srcString case: dstString.
	finder compareLines; compute.
	answer _ srcString class new asText.
	src1 _ srcString class writeStream.
	dst1 _ srcString class writeStream.
	changedCount _ 0.
	finder differences sort first do: [:item :condition |
		condition caseOf: {
			[ #unchanged ] -> [
				changedCount > 0 ifTrue: [
					"If the sequence of changed lines is large, comparing words gets too slow and less useful"
					changedCount > 30 ifTrue: [
						^nil ].
					"Compare the just ended sequence of changed lines"
					finder base: src1 contents case: dst1 contents.
					finder compareWords; compute: true.
					finder differences ifNil: [ ^nil ].
					answer _ answer append:  finder differences anyOne asText.
					src1 resetToStart.
					dst1 resetToStart.
					changedCount _ 0.
				].
				"This line hasn't changed. Just add it to the result in plain text."
				answer append: item ].
			[ #removed ] -> [
				"A removed line belongs in the source"
				src1 nextPutAll: item.
				changedCount _ changedCount + 1 ].
			[ #inserted ] -> [
				"An added line belongs in the destination"
				dst1 nextPutAll: item.
				changedCount _ changedCount + 1  ].
			}.
		].
	"If the sequence of changed lines is large, comparing words gets too slow and less useful"
	changedCount > 30 ifTrue: [
		^nil ].
	finder base: src1 contents case: dst1 contents.
	finder compareWords; compute: true.
	finder differences ifNil: [ ^nil ].
	answer _ answer append: finder differences anyOne asText.

	^answer! !

!methodRemoval: Utf8EncodedWriteStream #utf8Contents stamp: 'jmv 12/7/2022 09:42:28'!
Utf8EncodedWriteStream removeSelector: #utf8Contents!
!methodRemoval: UniFileStream #useUtf8String stamp: 'jmv 12/7/2022 09:18:37'!
UniFileStream removeSelector: #useUtf8String!
!methodRemoval: Text #asUtf8StringOrText stamp: 'jmv 12/7/2022 09:15:55'!
Text removeSelector: #asUtf8StringOrText!
!methodRemoval: UnicodeString #isUtf8String stamp: 'jmv 12/7/2022 09:18:12'!
UnicodeString removeSelector: #isUtf8String!
!methodRemoval: UnicodeString #asUtf8String stamp: 'jmv 12/7/2022 09:14:18'!
UnicodeString removeSelector: #asUtf8String!
!methodRemoval: CharacterSequence #asUtf8StringOrText stamp: 'jmv 12/7/2022 09:15:54'!
CharacterSequence removeSelector: #asUtf8StringOrText!
!methodRemoval: CharacterSequence #asUtf8String stamp: 'jmv 12/7/2022 09:14:17'!
CharacterSequence removeSelector: #asUtf8String!
!methodRemoval: Object #isUtf8String stamp: 'jmv 12/7/2022 09:18:11'!
Object removeSelector: #isUtf8String!

!UniFileStream reorganize!
('open/close' close closed ensureOpen open:forWrite: openReadOnly reopen)
('properties-setting' isReadOnly readOnly readWrite)
('access' directory fileEntry fullName localName name peekFor: printOn: reset size)
('read, write, position' atEnd crc16 flush next next: next:putAll:startingAt: nextBytes:into:startingAt: nextPut: nextPutAll: nextPutCodePoint: nextUtf8BytesAndCodePointInto:into: nextWordsInto: padToEndIfCantTruncate peek peekLast position position: readInto:startingAt:count: resetToStart setToEnd skip skip: skipBack truncate truncate:)
('testing' isDirectory isFileStream)
('primitives' primAtEnd: primClose: primCloseNoError: primFlush: primGetPosition: primOpen:writable: primRead:into:startingAt:count: primSetPosition:to: primSize: primSizeNoError: primTruncate:to: primWrite:from:startingAt:count:)
('registry' register unregister)
('finalization' actAsExecutor finalize)
('private' bytesUpToEnd disableReadBuffering enableReadBuffering flushReadBuffer nextByte nextBytes: nextCodePoint nextPutByte: pastEndPut:)
('printing' longPrintOn: longPrintOn:limitedTo:indent: storeOn:)
('accessing' collectionSpecies contents contentsOfEntireFile upToEnd)
('fileIn/Out' fileIn)
('reading modes' isBinary useBytes useCharacters useUnicodeString)
!

