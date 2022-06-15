'From Cuis 6.0 [latest update: #5262] on 11 June 2022 at 5:18:55 pm'!

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/11/2022 15:50:52'!
asUppercase
self flag: #unicodeNeedsImprovement.
	self asCharacter ifNotNil: [ :ch | ^UnicodeCodePoint codePoint: ch asUppercase codePoint ].
	^self! !


!CharacterSequence methodsFor: 'converting' stamp: 'jmv 11/30/2010 19:39'!
asCamelCase
	"Answer a new String, without any whitespace, and with words capitalized (Except for the first one)
	' how do you do? ' asCamelCase
	' how do you do? ' asUtf8String asCamelCase
	"
	^ String streamContents: [ :outStream | | inStream capitalize wroteSome |
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

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 6/11/2022 15:51:30'!
asUppercase
	"Answer a String made up from the receiver whose characters are all 
	uppercase."

	^ self collect: [ :each | each asUppercase ].! !

!CharacterSequence methodsFor: 'converting' stamp: 'sma 5/20/2000 17:17'!
romanNumber
	"
	2022 printStringRoman romanNumber
	"
	| value v1 v2 |
	value _ v1 _ v2 _ 0.
	self reverseDo:
		[:each |
		v1 _ #(1 5 10 50 100 500 1000) at: ('IVXLCDM' indexOf: each).
		v1 >= v2
			ifTrue: [value _ value + v1]
			ifFalse: [value _ value - v1].
		v2 _ v1].
	^ value! !

!CharacterSequence methodsFor: 'text conversion emphasis' stamp: 'len 6/3/2019 10:24:00'!
bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText bold! !

!CharacterSequence methodsFor: 'text conversion emphasis' stamp: 'len 6/3/2019 10:24:16'!
italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText italic! !

!CharacterSequence methodsFor: 'text conversion emphasis' stamp: 'jmv 9/3/2019 10:17:21'!
pointSize: pointSize
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	^self asText pointSize: pointSize! !

!CharacterSequence methodsFor: 'text conversion emphasis' stamp: 'len 6/6/2016 02:14'!
struck
	^self asText struck! !

!CharacterSequence methodsFor: 'text conversion emphasis' stamp: 'len 6/3/2019 10:24:18'!
sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText sub! !

!CharacterSequence methodsFor: 'text conversion emphasis' stamp: 'len 6/3/2019 10:24:20'!
super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText super! !

!CharacterSequence methodsFor: 'text conversion emphasis' stamp: 'len 6/3/2019 10:24:24'!
under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  Å' sub, '(1 / z)' ) edit
	"
	^self asText under! !

!CharacterSequence methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:08:21'!
centered
	"Stuff like
	('Hello world' centered ) edit
	"
	^self asText centered! !

!CharacterSequence methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:08:52'!
justified
	"Stuff like
	('Hello world' justified ) edit
	"
	^self asText justified! !

!CharacterSequence methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:09:50'!
leftFlush
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	^self asText leftFlush! !

!CharacterSequence methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:10:05'!
rightFlush
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	^self asText rightFlush! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:29:49'!
black
	"Stuff like
	'Hello world' black edit
	"
	^self asText black! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:29:58'!
blue
	"Stuff like
	'Hello world' blue edit
	"
	^self asText blue! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:30:04'!
cyan
	"Stuff like
	'Hello world' cyan edit
	"
	^self asText cyan! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:30:11'!
gray
	"Stuff like
	'Hello world' gray edit
	"
	^self asText gray! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:30:19'!
green
	"Stuff like
	'Hello world' green edit
	"
	^self asText green! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:31:04'!
magenta
	"Stuff like
	'Hello world' magenta edit
	"
	^self asText magenta! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:31:17'!
red
	"Stuff like
	'Hello world' red edit
	"
	^self asText red! !

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:31:27'!
yellow
	"Stuff like
	'Hello world' yellow edit
	"
	^self asText yellow! !


!Integer methodsFor: 'printing' stamp: 'nice 12/13/2009 17:59'!
printStringRoman
	"
	2022 printStringRoman romanNumber
	"
	^String streamContents: [:stream | | integer |
		integer := self negative ifTrue: [stream nextPut: $-. self negated] ifFalse: [self].
		integer // 1000 timesRepeat: [stream nextPut: $M].
		integer
			romanDigits: 'MDC' for: 100 on: stream;
			romanDigits: 'CLX' for: 10 on: stream;
			romanDigits: 'XVI' for: 1 on: stream]! !

!methodRemoval: String #leftFlush stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #leftFlush!
!methodRemoval: String #asCamelCase stamp: 'jmv 6/11/2022 17:18:15'!
String removeSelector: #asCamelCase!
!methodRemoval: String #cyan stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #cyan!
!methodRemoval: String #pointSize: stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #pointSize:!
!methodRemoval: String #struck stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #struck!
!methodRemoval: String #justified stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #justified!
!methodRemoval: String #romanNumber stamp: 'jmv 6/11/2022 17:17:52'!
String removeSelector: #romanNumber!
!methodRemoval: String #black stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #black!
!methodRemoval: String #super stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #super!
!methodRemoval: String #yellow stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #yellow!
!methodRemoval: String #blue stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #blue!
!methodRemoval: String #centered stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #centered!
!methodRemoval: String #green stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #green!
!methodRemoval: String #sub stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #sub!
!methodRemoval: String #bold stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #bold!
!methodRemoval: String #italic stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #italic!
!methodRemoval: String #magenta stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #magenta!
!methodRemoval: String #gray stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #gray!
!methodRemoval: String #under stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #under!
!methodRemoval: String #red stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #red!
!methodRemoval: String #rightFlush stamp: 'jmv 6/11/2022 17:17:05'!
String removeSelector: #rightFlush!

!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findLastOccurrenceOfString:startingAt: findString: findString:in:startingAt: findString:startingAt: findString:startingAt:caseSensitive: findTokens:keep: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: lines size skipAnySubStr:startingAt: string:lineIndicesDo:)
('comparing' < <= = > >= alike: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPrefixWith: compare: compare:caseSensitive: crc16 hash is:sameAs: is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('converting' asByteString asDate asFileName asHex asIdentifier: asLowercase asMonth asNumber asSmalltalkComment asUnHtml asUnaccented asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asUtf8BytesOrByteString asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: findPositiveInteger fixLineEndings initialIntegerOrNil keywords prefixAndSuffix: squeezedTo: string:withLineEndings: substrings substrings: surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateWithElipsisTo: uncapitalized withBlanksCondensed withCuisLineEndings withDescriptiveLineEndings withLineEndings: withoutPrefix: withoutSeparators withoutTrailingBlanks)
('enumerating' collect:)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with: compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAlphaNumeric isAscii isByteString isEmpty isRemote)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('formatting' format:)
('uCompletion' separateKeywords)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator isAbsolutePathName isDriveName)
('fileman-actions' fileContents fileContents:)
('fileman-accessing' indexOfLastPathSeparator pathAndLocalName upToLastPathSeparator)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
!

