'From Cuis 6.0 [latest update: #5512] on 28 October 2022 at 12:49:38 pm'!

!CodeProvider class methodsFor: 'services' stamp: 'jmv 10/28/2022 12:43:29'!
normalizeCodeForCodeFiles: aTextOrString
	"Adapt edited code to be suitable or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' and/or '_' for assignment, '^f' or return, and other replacements of
	ASCII by Unicode symbols done by Shout."

	| answer |
	answer _ aTextOrString asPlainString.
	answer _ answer copyReplaceAll: Character smalltalkLeftArrow asString with: '_'.
	answer _ answer copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	^answer! !


!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 12:11:21'!
smalltalkDownArrow
	"
	Character smalltalkDownArrow
	"
	^ Character numericValue: 31! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 12:10:24'!
smalltalkLeftArrow
	"
	Character smalltalkLeftArrow
	"
	^ Character numericValue: 28! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 12:11:02'!
smalltalkRightArrow
	"
	Character smalltalkRightArrow
	"
	^ Character numericValue: 29! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 10/28/2022 12:10:47'!
smalltalkUpArrow
	"
	Character smalltalkUpArrow
	"
	^ Character numericValue: 30! !


!CodeProvider class methodsFor: 'services' stamp: 'jmv 10/28/2022 12:16:08'!
normalizeCode: aTextOrString
	"Adapt edited code to be suitable for compilation or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' and/or '_' for assignment, and other replacements of
	ASCII by Unicode symbols done by Shout.
	WARNING: see handling of ReparseAfterSourceEditing and use of `requestor text string` in Parser.
	It seems that this method can not change the size of a method. Only one character for another!!"

	| answer |
	answer _ aTextOrString asPlainString.
	"
	answer _ answer copyReplaceAll: TextModel arrowStringForAssignment with: '_'.
	answer _ answer copyReplaceAll: TextModel arrowStringForReturn with: '^'.
	"
	^answer! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 10/28/2022 12:44:38'!
compile: code classified: category withStamp: changeStamp notifying: requestor logSource: logSource
	
	| methodAndNode selector isExistingMethod updatedCode |	
	methodAndNode _ self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector _ methodAndNode selector.
	isExistingMethod _ self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed']]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode _ requestor ifNil: [code] ifNotNil: [CodeProvider normalizeCodeForCodeFiles: requestor text].
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: methodAndNode 
		method inProtocol: category notifying: requestor.
	^ methodAndNode selector! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 10/28/2022 12:45:08'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil overridenMethodOrNil |

	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.
	overridenMethodOrNil := self superclass ifNotNil: [ :aSuperclass | 
		aSuperclass lookupSelector: aCompiledMethodWithNode selector ].

	aCompiledMethodWithNode method
		putSource: code
		fromParseNode: aCompiledMethodWithNode node
		class: self 
		category: category 
		withStamp: changeStamp 
		inFile: 2 
		priorMethod: priorMethodOrNil 
		overridesMethod: overridenMethodOrNil.! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 10/28/2022 12:12:12'!
codePoint: codePoint trySimilar: aBoolean
	"
	Answer an appropriante Character.
	If we don't have a suitable Character for codePoint, answer nil.
	But if aBoolean, answer a similar Character if possible.

	self assert: (Character codePoint: 16r41 convertToASCII: false) = $A.
	self assert: (Character codePoint: 16r20AC convertToASCII: false) = $§.
	"
	| codePoints similarCharacters |
	aBoolean ifTrue: [
		(codePoint between: 16r2018 and: 16r201B) ifTrue: [
			^$' ].
		(codePoint between: 16r201C and: 16r201F) ifTrue: [
			^$" ].
		(codePoint between: 16r2000 and: 16r200D) ifTrue: [
			^$  ].
		(codePoint between: 16r2010 and: 16r2015) ifTrue: [
			^$- ].
		(codePoint between: 16r2024 and: 16r2026) ifTrue: [
			^$- ].
		codePoint = 16r2190 ifTrue: [ ^ Character smalltalkLeftArrow ].
		codePoint = 16r2191 ifTrue: [ ^ Character smalltalkUpArrow ].
		codePoint = 16r2192 ifTrue: [ ^ Character smalltalkRightArrow ].
		codePoint = 16r2193 ifTrue: [ ^ Character smalltalkDownArrow ].
		codePoints _ #(16r2022 16r2023 16r2027 16r2032 16r2033 16r2035 16r2036 16r2039 16r203A).
		similarCharacters _ #($∞ $∞ $- $'  $" $` $" $< $>).
		(codePoints statePointsTo: codePoint) ifTrue: [
			^ similarCharacters at:  (codePoints indexOf: codePoint) ]].

	^ (self iso8859s15CodeForUnicodeCodePoint: codePoint)
		ifNotNil: [ :code | Character numericValue: code ]! !


!Scanner class methodsFor: 'class initialization' stamp: 'jmv 10/28/2022 12:20:15'!
initialize
	"
	Scanner initialize
	"
	self initTypeTable.
	self initializeDoitCharacter! !

!Scanner class methodsFor: 'cached class state' stamp: 'jmv 10/28/2022 12:27:50'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable atAll:  '°!!%&+-*/\∑◊˜¨≠Ø,<=>´ªø?@~êëíìîïûüôöõúù' asByteArray put: #xBinary.
	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $` numericValue put: #xBacktick.
	newTable at: $: numericValue put: #xColon.
	newTable at: $( numericValue put: #leftParenthesis.
	newTable at: $) numericValue put: #rightParenthesis.
	newTable at: $. numericValue put: #period.
	newTable at: $; numericValue put: #semicolon.
	newTable at: $[ numericValue put: #leftBracket.
	newTable at: $] numericValue put: #rightBracket.
	newTable at: ${ numericValue put: #leftBrace.
	newTable at: $} numericValue put: #rightBrace.
	newTable at: $^ numericValue put: #upArrow.
	newTable at: $_ numericValue put: #xUnderscore.
	newTable at: $| numericValue put: #verticalBar.
	newTable at: Character smalltalkLeftArrow numericValue put: #leftArrow.
	newTable at: Character smalltalkUpArrow numericValue put: #upArrow.
	newTable at: Character smalltalkRightArrow numericValue put: #xBinary.
	newTable at: Character smalltalkDownArrow numericValue put: #xBinary.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 10/28/2022 12:19:57'!
isAssignment

	^ currentToken = ':=' or: [ currentToken = '_' or: [ currentToken = Character smalltalkLeftArrow asString ]]! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 10/28/2022 12:13:51'!
parseStatement
	currentTokenFirst = $^ ifTrue: [self scanPast: #return].
	currentTokenFirst = Character smalltalkUpArrow ifTrue: [self scanPast: #return].
	self parseExpression! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 10/28/2022 12:17:11'!
privateFormatAndConvert

	"Turn := into an arrow, only if requested"
	(Preferences at: #syntaxHighlightingAsYouTypeLeftArrowAssignment) ifTrue: [
		self replaceStringForRangesWithType: #ansiAssignment with: Character smalltalkLeftArrow asString ].

	"Turn $_ either into an arrow or into := "
	(Preferences at: #syntaxHighlightingAsYouTypeAnsiAssignment)
		ifTrue: [ self replaceStringForRangesWithType: #assignment with: ':=' ]
		ifFalse: [ self replaceStringForRangesWithType: #assignment with: Character smalltalkLeftArrow asString ].

	self replaceStringForRangesWithType: #return with: Character smalltalkUpArrow asString.! !

!methodRemoval: TextModel class #arrowStringForAssignment stamp: 'jmv 10/28/2022 12:21:47'!
TextModel class removeSelector: #arrowStringForAssignment!
!methodRemoval: TextModel class #arrowStringForReturn stamp: 'jmv 10/28/2022 12:21:58'!
TextModel class removeSelector: #arrowStringForReturn!
Scanner initialize!
