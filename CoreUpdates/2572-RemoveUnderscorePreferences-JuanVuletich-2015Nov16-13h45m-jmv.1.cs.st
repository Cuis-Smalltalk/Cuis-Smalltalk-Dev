'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 16 November 2015 at 3:06:18.611117 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 11/16/2015 14:50'!
isValidInIdentifiers
	"Can c be part of an identifier? (unary or keyword selector, or variable name)"

	^self isAlphaNumeric or: [ self  = $_ ]! !

!Character methodsFor: 'testing' stamp: 'jmv 11/16/2015 14:56'!
isValidStartOfIdentifiers
	"Can c be the first character of an identifier? (unary or keyword selector, or variable name)"

	^self isLetter or: [ self  = $_ ]! !


!SequenceableCollection methodsFor: 'private' stamp: 'jmv 11/16/2015 14:52'!
copyReplaceAll: oldSubstring with: newSubstring asTokens: ifTokens
	"Answer a copy of the receiver in which all occurrences of
	oldSubstring have been replaced by newSubstring.
	ifTokens (valid for Strings only) specifies that the characters
	surrounding the recplacement must not be alphanumeric.
		Bruce Simth,  must be incremented by 1 and not 
	newSubstring if ifTokens is true.  See example below. "

	| aString startSearch currentIndex endIndex |
	(ifTokens and: [ self isString not ])
		ifTrue: [(self is: #Text) ifFalse: [
			self error: 'Token replacement only valid for Strings']].
	aString _ self.
	startSearch _ 1.
	[(currentIndex _ aString indexOfSubCollection: oldSubstring startingAt: startSearch)
			 > 0]
		whileTrue: 
		[endIndex _ currentIndex + oldSubstring size - 1.
		(ifTokens not
			or: [(currentIndex = 1
					or: [(aString at: currentIndex-1) isValidInIdentifiers not])
				and: [endIndex = aString size
					or: [(aString at: endIndex+1) isValidInIdentifiers not]]])
			ifTrue: [aString _ aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newSubstring.
				startSearch _ currentIndex + newSubstring size]
			ifFalse: [
				ifTokens 
					ifTrue: [startSearch _ currentIndex + 1]
					ifFalse: [startSearch _ currentIndex + newSubstring size]]].
	^ aString

"Test case:
	'test te string' copyReplaceAll: 'te' with: 'longone' asTokens: true   "
! !


!String methodsFor: 'converting' stamp: 'jmv 11/16/2015 14:57'!
asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString firstChar firstLetterPosition |
	aString _ self select: [ :el | el isValidInIdentifiers ].
	firstLetterPosition _ aString findFirst: [ :ch | ch isValidStartOfIdentifiers ].
	aString _ firstLetterPosition = 0
		ifFalse: [
			aString copyFrom: firstLetterPosition to: aString size]
		ifTrue: [
			'a', aString].
	firstChar _ shouldBeCapitalized ifTrue: [aString first asUppercase] ifFalse: [aString first asLowercase].

	^ firstChar asString, (aString copyFrom: 2 to: aString size)
"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false

"! !

!String methodsFor: 'converting' stamp: 'jmv 11/16/2015 14:57'!
asLegalSelector
	| toUse |
	toUse _ ''.
	self do:
		[:char | char isValidInIdentifiers ifTrue: [toUse _ toUse copyWith: char]].
	(self size = 0 or: [self first isValidStartOfIdentifiers not])
		ifTrue:		[toUse _ 'v', toUse].

	^ toUse withFirstCharacterDownshifted

"'234znak 43 ) 2' asLegalSelector"! !

!String methodsFor: 'system primitives' stamp: 'jmv 11/16/2015 14:57'!
numArgs 
	"Answer either the number of arguments that the receiver would take if considered a selector.  Answer -1 if it couldn't be a selector.  Note that currently this will answer -1 for anything begining with an uppercase letter even though the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."

	| firstChar numColons excess start ix |
	self size = 0 ifTrue: [^ -1].
	firstChar _ self at: 1.
	(firstChar isValidStartOfIdentifiers or: [firstChar = $:]) ifTrue:
		["Fast reject if any chars are non-alphanumeric"
		(self findSubstring: '~' in: self startingAt: 1 matchTable: Tokenish) > 0 ifTrue: [^ -1].
		"Fast colon count"
		numColons _ 0.  start _ 1.
		[(ix _ self findSubstring: ':' in: self startingAt: start matchTable: CaseSensitiveOrder) > 0]
			whileTrue:
				[numColons _ numColons + 1.
				start _ ix + 1].
		numColons = 0 ifTrue: [^ 0].
		firstChar = $:
			ifTrue: [excess _ 2 "Has an initial keyword, as #:if:then:else:"]
			ifFalse: [excess _ 0].
		self last = $:
			ifTrue: [^ numColons - excess]
			ifFalse: [^ numColons - excess - 1 "Has a final keywords as #nextPut::andCR"]].
	firstChar isSpecial ifTrue:
		[self size = 1 ifTrue: [^ 1].
		2 to: self size do: [:i | (self at: i) isSpecial ifFalse: [^ -1]].
		^ 1].
	^ -1.! !


!Symbol class methodsFor: 'access' stamp: 'jmv 11/16/2015 14:53'!
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList _ OrderedCollection new.
	(size _ aString size) = 0 ifTrue: [^selectorList].

	aString size = 1 ifTrue:
		[
			ascii _ aString first asciiValue.
			ascii < 128 ifTrue: [selectorList add: (OneCharacterSymbols at: ascii+1)]
		].

	aString first isValidInIdentifiers ifFalse:
		[
			aString size = 2 ifTrue: 
				[Symbol hasInterned: aString ifTrue:
					[:s | selectorList add: s]].
			^selectorList
		].

	selectorList _ selectorList copyFrom: 2 to: selectorList size.

	self allSymbolTablesDo: [:each |
		each size >= size ifTrue:
			[(each findSubstring: aString in: each startingAt: 1 
				matchTable: CaseInsensitiveOrder) > 0
						ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asString withFirstCharacterDownshifted numArgs < 0]].

"Symbol selectorsContaining: 'scon'"! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 11/16/2015 14:51'!
keywordsAndBlanksFrom: aString
	^Array streamContents: [:strm | | read keyword tail |
		read := aString readStream.
		[read atEnd] whileFalse: [
			keyword := read nextKeyword.
			keyword notEmpty ifTrue: [
				strm nextPut: keyword ].
			tail := read untilAnySatisfying: [:ch | ch isValidInIdentifiers].
			tail notEmpty ifTrue: [strm nextPut: tail]]]
! !


!Editor methodsFor: 'new selection' stamp: 'jmv 11/16/2015 14:51'!
selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| openDelimiter closeDelimiter direction match level
	string here hereChar start stop |
	string _ self privateCurrentString.
	string size < 2 ifTrue: [^self selectFrom: 1 to: 1].
	here _ self pointIndex min: string size max: 2.
	openDelimiter _ string at: here - 1.
	match _ leftDelimiters indexOf: openDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start _ here.
			direction _ 1.
			here _ here - 1.
			closeDelimiter _ rightDelimiters at: match]
		ifFalse: [
			openDelimiter _ string at: here.
			match _ rightDelimiters indexOf: openDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop _ here - 1.
					direction _ -1.
					closeDelimiter _ leftDelimiters at: match]
				ifFalse: [
					"no delimiters -- select a token"
					direction _ -1]].
	level _ 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < string size]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar _ string at: (here _ here + direction).
			match = 0
				ifTrue: ["token scan goes left, then right"
					hereChar isValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [here = 1
								ifTrue: [
									start _ 1.
									"go right if hit string start"
									direction _ 1]]
						ifFalse: [
							direction < 0
								ifTrue: [
									start _ here + 1.
									"go right if hit non-token"
									direction _ 1]
								ifFalse: [level _ 0]]]
				ifFalse: ["bracket match just counts nesting level"
					hereChar = closeDelimiter
						ifTrue: [level _ level - 1"leaving nest"]
						ifFalse: [hereChar = openDelimiter 
									ifTrue: [level _ level + 1"entering deeper nest"]]]].

	level > 0 ifTrue: ["in case ran off string end"	here _ here + direction].
	direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here _ here + 1].
			self selectFrom: start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			self selectFrom: here + 1 to: stop]! !

!Editor methodsFor: 'private' stamp: 'jmv 11/16/2015 14:51'!
nextWordEnd: position
	| string index |
	string _ self privateCurrentString.
	index _ position.
	[ (index between: 1 and: string size) and: [(string at: index) isValidInIdentifiers not ]]
		whileTrue: [ index _ index + 1 ].
	[ (index between: 1 and: string size) and: [(string at: index) isValidInIdentifiers ]]
		whileTrue: [ index _ index + 1 ].
	^ index! !

!Editor methodsFor: 'private' stamp: 'jmv 11/16/2015 14:51'!
nextWordStart: position
	| string index |
	string _ self privateCurrentString.
	index _ position.
	[ (index between: 1 and: string size) and: [(string at: index) isValidInIdentifiers ]]
		whileTrue: [ index _ index + 1 ].
	[ (index between: 1 and: string size) and: [(string at: index) isValidInIdentifiers not ]]
		whileTrue: [ index _ index + 1 ].
	^ index! !

!Editor methodsFor: 'private' stamp: 'jmv 11/16/2015 14:51'!
previousWordStart: position
	| string index |
	string _ self privateCurrentString.
	index _ position.
	[ (index between: 1 and: string size) and: [ (string at: index) isValidInIdentifiers not ]]
		whileTrue: [ index _ index - 1 ].
	[ (index between: 1 and: string size) and: [ (string at: index) isValidInIdentifiers ]]
		whileTrue: [ index _ index - 1 ].
	^ index + 1! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/16/2015 14:52'!
explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
	sorry _ '"Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	sorry _ sorry , (morph canDiscardEdits
		ifFalse: ['  Also, please cancel or accept."']
		ifTrue: ['"']).
	(string _ self selection asString) isEmpty
		ifTrue: [reply _ '']
		ifFalse: [
			string _ string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars _ (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider _ self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars _ provider explainSpecial: string].
			tiVars _ tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol _ s])
				ifTrue: [
					cgVars _ (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors _ (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors _ self explainPartSel: string].
			cgVars _ cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors _ selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimitors _ string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimitors"
					self explainDelimitor: string].
			numbers _ self explainNumber: string.
			numbers ifNil: [numbers _ ''].
			delimitors ifNil: [delimitors _ ''].
			reply _ tiVars , cgVars , selectors , delimitors , numbers].
	reply size = 0 ifTrue: [reply _ sorry].
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print! !


!Character methodsFor: 'testing' stamp: 'jmv 11/16/2015 14:51'!
tokenish
	"Answer whether the receiver is a valid token-character--letter, digit, or 
	colon."

	^self isValidInIdentifiers or: [self = $:]! !


!AssignmentNode methodsFor: 'printing' stamp: 'jmv 11/16/2015 15:04'!
printOn: aStream indent: level 
	variable printOn: aStream indent: level.
	aStream nextPutAll: ' _ '.
	value printOn: aStream indent: level! !

!AssignmentNode methodsFor: 'printing' stamp: 'jmv 11/16/2015 15:04'!
printWithClosureAnalysisOn: aStream indent: level 
	variable printWithClosureAnalysisOn: aStream indent: level.
	aStream nextPutAll: ' _ '.
	value printWithClosureAnalysisOn: aStream indent: level! !


!Preferences class methodsFor: 'themes' stamp: 'jmv 11/16/2015 15:05'!
cuisDefaults
	"
	Preferences cuisDefaults
	"
	self setPreferencesFrom:

	#(
		(balloonHelpEnabled true)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds false)
		(checkForSlips true)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl true)
		(optionalButtons true)
		(extraDebuggerButtons true)
		(smartUpdating true)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe false)
		(syntaxHighlightingAsYouType true)
		(tapAndHoldEmulatesButton2 true)
		(clickGrabsMorphs false)

		(syntaxHighlightingAsYouTypeAnsiAssignment false)
		(syntaxHighlightingAsYouTypeLeftArrowAssignment false)
	).
	self useAllIcons
	".
	Theme beCurrent.
	Taskbar showTaskbar
	"! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 11/16/2015 14:54'!
scanIdentifier
	| c start |
	start _ sourcePosition.
	[ (c _ self nextChar) isValidInIdentifiers ] whileTrue: [].
	(c == $: and: [(self isSelectorCharacter: self peekChar) not]) 
		ifTrue: [self nextChar].
	currentToken _ source copyFrom: start to: sourcePosition - 1.
	currentTokenSourcePosition _ start! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 11/16/2015 14:57'!
scanNext
	self scanWhitespace.
	currentTokenFirst _ self currentChar.
	currentTokenFirst 
		ifNil: [" end of input "
			currentTokenFirst _ $ .
			currentTokenSourcePosition _ nil.
			currentToken _ nil.
			^nil].
	currentTokenFirst isDigit ifTrue: [^self scanNumber].
	currentTokenFirst isValidStartOfIdentifiers ifTrue: [^self scanIdentifier].
	^self scanBinary! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 11/16/2015 15:03'!
isAssignment
	^currentToken = ':=' or: [ currentToken = '_' ]! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 11/16/2015 14:57'!
isKeyword
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last == $: ]! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 11/16/2015 14:57'!
isName
	^ currentTokenFirst isValidStartOfIdentifiers and: [ currentToken last isValidInIdentifiers ]! !

!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 11/16/2015 14:54'!
isSelectorCharacter: aCharacter
	"Actually meaning 'isBinarySelectorCharacter'. i.e. can aCharacter be part of a binary selector? "
	aCharacter isValidInIdentifiers ifTrue: [^false].
	aCharacter isSeparator ifTrue: [^false].
	"$- is specified here as NOT being a selector char, but it can appear as the 
	first char in a binary selector. That case is handled specially elsewhere"
	('"#$'':().;[]{}^_-'  includes: aCharacter) 
		ifTrue:[^false].
	aCharacter asciiValue = 30 ifTrue: [^false "the doIt char"].
	aCharacter asciiValue = 0 ifTrue: [^false].
	"Any other char is ok as a binary selector char."
	^true
! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 11/16/2015 14:57'!
parseLiteralArrayElement
	 currentTokenFirst isValidStartOfIdentifiers ifTrue: [
		#true = currentToken ifTrue: [
			self scanPast: #true.
			^ self ].
		#false = currentToken ifTrue: [
			self scanPast: #false.
			^ self ].
		#nil = currentToken ifTrue: [
			self scanPast: #nil.
			^ self ].
		self scanPast: #symbol.
		^ self ].
	currentTokenFirst == $( ifTrue: [
		self scanPast: #arrayStart.
		self parseArray.
		^ self ].
	self parseLiteral: true! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 11/16/2015 14:57'!
parseSymbol
	| c |
	currentToken = '#' 
		ifTrue: [
			"if token is just the #, then scan whitespace and comments
			and then process the next character.
			Allows space between the # and the start of the symbol 
			e.g. # (),  #  a, #  'sym' "
			self rangeType: #symbol.
			self scanWhitespace].
	c _ self currentChar.
	self failWhen: (c isNil or: [c isSeparator]).
	c == $( 
		ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseArray].
	c == $' ifTrue: [
		self parseSymbolString.
		^self ].
	c == $[ ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseByteArray].
	((self isSelectorCharacter: c) or: [c == $-]) ifTrue: [
		self parseSymbolSelector.
		^self ].
	( c isValidStartOfIdentifiers or: [c == $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 11/16/2015 14:57'!
parseSymbolIdentifier
	| c start end |
	c _ self currentChar.
	self failUnless: ( c isValidStartOfIdentifiers or: [ c == $: ]).
	start _ sourcePosition.
	[
		c _ self nextChar.
		c isValidInIdentifiers or: [ c == $: ]
	] whileTrue: [].
	end _ sourcePosition - 1.
	self scanPast: #symbol start: start - 1 end: end! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 11/16/2015 14:59'!
xLetter
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore]]] whileTrue:
			["open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
							ifTrue: [30 asCharacter "doit"]
							ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~~ $=]])
					ifTrue: 
						[buffer nextPut: self step.
						"Allow any number of embedded colons in literal symbols"
						[(self typeTableAt: hereChar) == #xColon] whileTrue:
							[buffer nextPut: self step].
						#keyword]
					ifFalse: 
						[#word].
	token := buffer contents! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 11/16/2015 15:04'!
xUnderscore
	| type |
	"Figure out if x _foo (no space between _ and foo) 
	should be a selector or assignment"
	((type := self typeTableAt: aheadChar) == #xLetter
		or:[type == #xDigit or:[type == #xUnderscore]]) ifFalse: [
			self step.
			tokenType := #leftArrow.
			^token := #'_'
	].
	^self xLetter! !


!Scanner class methodsFor: 'testing' stamp: 'jmv 11/16/2015 15:02'!
isLiteralSymbol: aSymbol 
	"Test whether a symbol can be stored as # followed by its characters.  
	Symbols created internally with asSymbol may not have this property, 
	e.g. '3' asSymbol."
	| i ascii type |
	i := aSymbol size.
	i = 0 ifTrue: [^ false].
	ascii := (aSymbol at: 1) asciiValue.
	"TypeTable should have been origined at 0 rather than 1 ..."
	ascii = 0 ifTrue: [^ false].
	type := self typeTable at: ascii ifAbsent: [#xLetter].
	type == #xBinary ifTrue:
		[^i <= 2
		  and: [i = 1 or: [(self typeTable at: (aSymbol at: 2) asciiValue ifAbsent: []) == #xBinary]]].
	(type == #xColon or: [type == #verticalBar]) ifTrue:
		[^i = 1].
	(type == #xLetter or: [type == #xUnderscore]) ifTrue: [
		[i > 1] whileTrue: [
			ascii := (aSymbol at: i) asciiValue.
			ascii = 0 ifTrue: [^false].
			type := self typeTable at: ascii ifAbsent: [#xLetter].
			((type == #xLetter or: [type == #xUnderscore])or: [type == #xDigit or: [type == #xColon]]) ifFalse: [
				^false].
			i := i - 1].
		^true].
	^false! !

!Scanner class methodsFor: 'testing' stamp: 'jmv 11/16/2015 14:57'!
wellFormedInstanceVariableNameFrom: aString
	"Answer a legal instance variable name, derived from aString"

	| cleansedString |
	cleansedString _ aString select: [:ch | ch isValidInIdentifiers].
	(cleansedString isEmpty or: [cleansedString first isValidStartOfIdentifiers not])
		ifTrue: [cleansedString _ 'a', cleansedString]
		ifFalse:	[cleansedString _ cleansedString withFirstCharacterDownshifted].

	[self isLegalInstVarName: cleansedString] whileFalse:
		[cleansedString _ cleansedString, 'x'].
	^ cleansedString

"Scanner wellFormedInstanceVariableNameFrom:  '234 xx\ Uml /ler42342380-4'"! !


!Parser class methodsFor: 'class initialization' stamp: 'jmv 11/16/2015 15:05'!
initialize
	"
	Parser initialize
	"
	Preferences
		addPreference: #allowBlockArgumentAssignment 
		category: #compiler 
		default: false
		balloonHelp: 'If enabled, the compiler will allow assignment into block arguments.\This provides backward compatibility with the pre-closure compiler.' withNewLines! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 11/16/2015 14:52'!
nextKeyword
	^self
		untilAnySatisfying: [:c | (c isValidInIdentifiers or: [c = $:]) not]! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 11/16/2015 14:57'!
wellFormedInstanceVariableNameFrom: aString
	"Answer a legal instance variable name, derived from aString"

	| cleansedString |
	cleansedString _ aString select: [:ch | ch isValidInIdentifiers].
	(cleansedString isEmpty or: [cleansedString first isValidStartOfIdentifiers not])
		ifTrue: [cleansedString _ 'a', cleansedString]
		ifFalse:	[cleansedString _ cleansedString withFirstCharacterDownshifted].

	[self isLegalInstVarName: cleansedString] whileFalse:
		[cleansedString _ cleansedString, 'x'].
	^ cleansedString

"Utilities wellFormedInstanceVariableNameFrom:  '234 xx\ Uml /ler42342380-4'"! !

Parser initialize!
!methodRemoval: Parser #allowUnderscoreAssignments!
Parser removeSelector: #allowUnderscoreAssignments!
!methodRemoval: Parser #allowUnderscoreSelectors!
Parser removeSelector: #allowUnderscoreSelectors!
!methodRemoval: Scanner #allowUnderscoreAssignments!
Scanner removeSelector: #allowUnderscoreAssignments!
!methodRemoval: Scanner #allowUnderscoreSelectors!
Scanner removeSelector: #allowUnderscoreSelectors!
!methodRemoval: SHParserST80 #allowUnderscoreAssignments!
SHParserST80 removeSelector: #allowUnderscoreAssignments!
!methodRemoval: SHParserST80 #allowUnderscoreSelectors!
SHParserST80 removeSelector: #allowUnderscoreSelectors!
!methodRemoval: SHParserST80 #isValidInIdentifiers:!
SHParserST80 removeSelector: #isValidInIdentifiers:!
!methodRemoval: SHParserST80 #isValidStartOfIdentifiers:!
SHParserST80 removeSelector: #isValidStartOfIdentifiers:!
!methodRemoval: Preferences class #allowUnderscoreAssignments!
Preferences class removeSelector: #allowUnderscoreAssignments!
!methodRemoval: Preferences class #allowUnderscoreSelectors!
Preferences class removeSelector: #allowUnderscoreSelectors!
!methodRemoval: Character #isGenerallyValidInIdentifiers!
Character removeSelector: #isGenerallyValidInIdentifiers!
!methodRemoval: Character #isGenerallyValidStartOfInIdentifiers!
Character removeSelector: #isGenerallyValidStartOfInIdentifiers!
!methodRemoval: Character #isValidStartOfInIdentifiers!
Character removeSelector: #isValidStartOfInIdentifiers!
!methodRemoval: Behavior #allowUnderscoreAssignments!
Behavior removeSelector: #allowUnderscoreAssignments!
!methodRemoval: Behavior #allowUnderscoreSelectors!
Behavior removeSelector: #allowUnderscoreSelectors!
