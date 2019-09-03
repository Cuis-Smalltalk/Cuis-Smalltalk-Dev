'From Cuis 5.0 [latest update: #3854] on 3 September 2019 at 11:58:35 am'!

!String methodsFor: 'text conversion emphasis' stamp: 'jmv 9/3/2019 10:17:21'!
pointSize: pointSize
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	^self asText pointSize: pointSize! !

!String methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:08:21'!
centered
	"Stuff like
	('Hello world' centered ) edit
	"
	^self asText centered! !

!String methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:08:52'!
justified
	"Stuff like
	('Hello world' justified ) edit
	"
	^self asText justified! !

!String methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:09:50'!
leftFlush
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	^self asText leftFlush! !

!String methodsFor: 'text conversion attributes' stamp: 'jmv 9/3/2019 10:10:05'!
rightFlush
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	^self asText rightFlush! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:29:49'!
black
	"Stuff like
	'Hello world' black edit
	"
	^self asText black! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:29:58'!
blue
	"Stuff like
	'Hello world' blue edit
	"
	^self asText blue! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:30:04'!
cyan
	"Stuff like
	'Hello world' cyan edit
	"
	^self asText cyan! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:30:11'!
gray
	"Stuff like
	'Hello world' gray edit
	"
	^self asText gray! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:30:19'!
green
	"Stuff like
	'Hello world' green edit
	"
	^self asText green! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:31:04'!
magenta
	"Stuff like
	'Hello world' magenta edit
	"
	^self asText magenta! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:31:17'!
red
	"Stuff like
	'Hello world' red edit
	"
	^self asText red! !

!String methodsFor: 'text conversion color' stamp: 'jmv 9/3/2019 10:31:27'!
yellow
	"Stuff like
	'Hello world' yellow edit
	"
	^self asText yellow! !


!Text methodsFor: 'adding emphasis' stamp: 'jmv 9/3/2019 10:16:59'!
pointSize: pointSize
	"Stuff like
	('Hello World' pointSize: 22) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: (TextFontFamilyAndSize pointSize: pointSize) from: 1 to: string size! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 9/3/2019 10:08:33'!
centered
	"Stuff like
	('Hello world' centered ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextAlignment centered from: 1 to: string size! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 9/3/2019 10:08:58'!
justified
	"Stuff like
	('Hello world' justified ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextAlignment justified from: 1 to: string size! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 9/3/2019 10:09:57'!
leftFlush
	"Stuff like
	('Hello world' leftFlush ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextAlignment leftFlush from: 1 to: string size! !

!Text methodsFor: 'adding attributes' stamp: 'jmv 9/3/2019 10:10:10'!
rightFlush
	"Stuff like
	('Hello world' rightFlush ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextAlignment rightFlush from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:30:49'!
black
	"Stuff like
	'Hello world' black edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor black from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:30:43'!
blue
	"Stuff like
	'Hello world' blue edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor blue from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:30:38'!
cyan
	"Stuff like
	'Hello world' cyan edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor cyan from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:30:32'!
gray
	"Stuff like
	'Hello world' gray edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor gray from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:30:27'!
green
	"Stuff like
	'Hello world' green edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor green from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:31:00'!
magenta
	"Stuff like
	'Hello world' magenta edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor magenta from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:31:13'!
red
	"Stuff like
	'Hello world' red edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor red from: 1 to: string size! !

!Text methodsFor: 'adding color' stamp: 'jmv 9/3/2019 10:31:31'!
yellow
	"Stuff like
	'Hello world' yellow edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextColor yellow from: 1 to: string size! !


!Text methodsFor: 'emphasis' stamp: 'jmv 9/3/2019 11:44:38'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	| fn ps |
	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle :backgroundColor |
		fn _ familyName ifNil: [ defaultFont ifNotNil: [ defaultFont familyName ] ifNil: [ FontFamily defaultFamilyName ]].
		ps _ pointSize ifNil: [ defaultFont ifNotNil: [ defaultFont pointSize ] ifNil: [ FontFamily defaultPointSize ]].
		^((AbstractFont familyName: fn pointSize: ps) ifNil: [
			AbstractFont familyName: fn aroundPointSize: ps])
			emphasized: emphasis ]! !


!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('converting' aOrAnPrefix adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: afterBlanksEndsWith: asCamelCase asDate asFileName asHex asIdentifier: asInteger asLowercase asMonth asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector firstNoBlankIndex initialIntegerOrNil keywords lastNoBlankIndex prefixAndSuffix: prefixedWithAOrAn romanNumber squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withLineEndings: withNewLines withoutEnclosing: withoutLeadingBlanks withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect:)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral print printOn: storeOn:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer is: isAlphaNumeric isRemote isString)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('user interface' edit editLabel:)
('formatting' format:)
('uCompletion' separateKeywords)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-actions' fileContents fileContents:)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
!


!Text reorganize!
('accessing' append: at: at:put: commandForReplaceFrom:to:with: customizeExplorerContents embeddedMorphs embeddedMorphsFrom:to: findString:startingAt: findString:startingAt:caseSensitive: lineCount prepend: rangeOf:startingAt: replaceFrom:to:with: size string)
('comparing' = hash)
('copying' , appendToString: appendToText: copyFrom:to: copyReplaceFrom:to:with: copyReplaceTokens:with: postCopy)
('converting' asNumber asString asText displayStringOrText optimizedForMutationSpeed replaceFrom:to:with:startingAt: replaceFrom:to:withString:attributes:startingAt: reversed withCuisLineEndings)
('emphasis' addAttribute: addAttribute:from:to: alignmentAt: attributesAt: attributesFrom:to:do: characterStyleOrNilAt: characterStyleOrNilIfApplying: commandForAddAttribute:from:to: commandForRemoveAttributes:from:to: emphasisAt: find: font: fontAt:default: fontIfApplying:default: initialFont: initialFontFrom: paragraphStyleOrNilAt: paragraphStyleOrNilIfApplying: removeAttributes:from:to: removeAttributesThat: runLengthFor: withAttributeValues:do:)
('testing' is: isRemote)
('printing' printOn: storeOn:)
('private' basicReplaceAttributesFrom:to:with: fixParagraphAttributesFrom:to: privateSetParagraphAttributes:from:to: privateSetRuns: runs setString:setRuns:)
('attributes' canJoin:and: hasAnyAttribute hasAnyParagraphAttribute)
('user interface' edit editLabel:)
('paragraph support' encompassLine: encompassParagraph: paragraphStyleChunksDo:)
('adding emphasis' allBold bold italic pointSize: struck sub super under)
('adding attributes' centered justified leftFlush rightFlush)
('adding color' black blue cyan gray green magenta red yellow)
('adding' add:)
!

