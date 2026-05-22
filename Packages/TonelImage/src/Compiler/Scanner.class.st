"
I scan a string or text, picking out Smalltalk syntactic tokens. I look one character ahead. I put each token found into the instance variable, token, and its type (a Symbol) into the variable, tokenType. At the end of the input stream, I pretend to see an endless sequence of special characters called doIts.

Instance Variables
	aheadChar:		<Character>
	buffer:		<WriteStream>
	currentComment:		<OrderedCollection>
	hereChar:		<Character>
	mark:		<Integer>
	source:		<ReadStream>
	token:		<Symbol|String|NumberCharacter|Boolean|nil>
	tokenType:		<Symbol>
	typeTable:		<Array>

aheadChar
	- the next character in the input stream

buffer
	- a reusable WriteStream on a String which is used for building strings. Shouldn't be used from multiple methods without resetting.

currentComment
	- an OrderedCollection of strings which contain all comments between the current token and the previous token or the beginning of the source.

hereChar
	- the current character

mark
	- the position of the current token in the source stream

source
	- the input stream of characters

token
	- the current token

tokenType
	- the type of the current token. The possible token types are: #binary, #character, #colon, #doIt, #keyword, #leftArrow, #leftBrace, #leftBracket, #leftParenthesis, #literal, #period, #rightBrace, #rightBracket, #rightParenthesis, #semicolon, #string, #upArrow, #verticalBar, #word, #xBinary, #xColon, #xDelimiter, #xDigit, #xDollar, #xDoubleQuote, #xLetter, #xLitQuote, #xSingleQuote, #xUnderscore

typeTable
	- an array that maps each an evaluable tokenType to each character with asciiValue between 0 and 255

See Documentation/HitchHikersCompiler.md
"
Class {
	#name : 'Scanner',
	#superclass : 'Object',
	#instVars : [
		'source',
		'mark',
		'hereChar',
		'aheadChar',
		'token',
		'tokenType',
		'currentComment',
		'buffer',
		'typeTable',
		'isForTokenization',
		'sentInLiterals'
	],
	#classVars : [
		'DoItCharacter',
		'TypeTable'
	],
	#category : 'Compiler-Kernel',
	#package : 'Compiler',
	#tag : 'Kernel'
}

{ #category : 'cached class state' }
Scanner class >> doItCharacterValue [
	^26
]

{ #category : 'doIt selectors' }
Scanner class >> doItInSelector [

	^#DoItIn:
]

{ #category : 'doIt selectors' }
Scanner class >> doItSelector [

	^#DoIt
]

{ #category : 'testing' }
Scanner class >> findSelectorIn: aString [

    | result |
    result := Scanner new
        scan: aString readStream;
        scanForFindSelectorUpTo: #notATerminator.
    ^result
]

{ #category : 'testing' }
Scanner class >> findSelectorTests [
"
Scanner findSelectorTests explore
"
    ^#(
        '3 perform: #between:and: with: 1 with: 5'
        'self do: [:i | ] separatedBy: [] '
        'self perform: #add: with: anObject'
        'self do: [:i | i do: [ :each | ] ] separatedBy: [] '
        'self x: {self y:} y: ''self new: 100'''
        'a max:b'
        'a'
    ) collect: [ :e |
        {e. self findSelectorIn: e}
    ].
]

{ #category : 'cached class state' }
Scanner class >> initTypeTable [
	| newTable |
	newTable := Array new: 127 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: Character binarySelectorCharacters asByteArray put: #xBinary.
	1 to: 127 do: [:index |
		(Character codePoint: index) isLetter ifTrue: [
			newTable at: index put: #xLetter ]].
	newTable atAll: ($0 codePoint to: $9 codePoint) put: #xDigit.
	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" codePoint put: #xDoubleQuote.
	newTable at: $# codePoint put: #xLitQuote.
	newTable at: $$ codePoint put: #xDollar.
	newTable at: $' codePoint put: #xSingleQuote.
	newTable at: $` codePoint put: #xBacktick.
	newTable at: $: codePoint put: #xColon.
	newTable at: $( codePoint put: #leftParenthesis.
	newTable at: $) codePoint put: #rightParenthesis.
	newTable at: $. codePoint put: #period.
	newTable at: $; codePoint put: #semicolon.
	newTable at: $[ codePoint put: #leftBracket.
	newTable at: $] codePoint put: #rightBracket.
	newTable at: ${ codePoint put: #leftBrace.
	newTable at: $} codePoint put: #rightBrace.
	newTable at: $^ codePoint put: #upArrow.
	newTable at: $_ codePoint put: #xUnderscore.
	newTable at: $| codePoint put: #verticalBar.
	newTable at: Character smalltalkLeftArrow codePoint put: #leftArrow.
	newTable at: Character smalltalkUpArrow codePoint put: #upArrow.
	TypeTable := newTable "bon voyage!"

	"
	Scanner initTypeTable
	"
]

{ #category : 'class initialization' }
Scanner class >> initialize [
	"
	Scanner initialize.
	"
	self initTypeTable.
	self initializeDoitCharacter.
]

{ #category : 'class initialization' }
Scanner class >> initializeDoitCharacter [

	DoItCharacter := Character codePoint: self doItCharacterValue 
]

{ #category : 'testing' }
Scanner class >> isValidInstanceVariableName: aString [
	"Answer whether aString is a legal instance variable name."

	^ ((Symbol isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self pseudoVariableNames includes:  aString) not]
]

{ #category : 'instance creation' }
Scanner class >> new [

	^self basicNew initScanner
]

{ #category : 'testing' }
Scanner class >> pseudoVariableNames [
	"Answer a list of Smalltalk pseudo-varialbes"
	^ #('nil' 'true' 'false' 'self' 'super' 'thisContext')
]

{ #category : 'cached class state' }
Scanner class >> releaseClassCachedState [
	TypeTable := nil
]

{ #category : 'cached class state' }
Scanner class >> typeTable [

	TypeTable ifNil: [
		self initTypeTable ].
	^TypeTable
]

{ #category : 'testing' }
Scanner class >> typeTable: aTypeTable at: aCharacterOrCodePoint [

	"Prebuilt tables for the ASCII range"
	aCharacterOrCodePoint isAscii ifTrue: [
		^aTypeTable at: aCharacterOrCodePoint asciiValue ].

	"For CodePoints not in the ASCII range"
	aCharacterOrCodePoint isDiacriticalMark ifTrue: [ ^#xDiacritical ].
	aCharacterOrCodePoint isLetter ifTrue: [ ^#xLetter ].
	aCharacterOrCodePoint isSymbolLike ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint == $­ ifTrue: [ ^#xBinary ].
	aCharacterOrCodePoint isSeparator ifTrue: [ ^#xDelimiter ].
	"Many should actually be #xIllegal"
	^#xLetter
]

{ #category : 'testing' }
Scanner class >> typeTableAt: aCharacterOrCodePoint [
	TypeTable ifNil: [
		self initTypeTable ].
	^self typeTable: TypeTable at: aCharacterOrCodePoint
]

{ #category : 'error descriptions' }
Scanner class >> unmatechedCommentQuoteErrorDescription [

	^'Unmatched comment quote'
]

{ #category : 'expression types' }
Scanner >> advance [

	| prevToken |
	prevToken := token.
	self scanToken.
	^prevToken
]

{ #category : 'expression types' }
Scanner >> checkpoint [
	"Return a copy of all changeable state.  See revertToCheckpoint:"

	^ {self copy. source copy. currentComment copy}
]

{ #category : 'error handling' }
Scanner >> errorMultibyteCharacter [

	self error: 'multi-byte character is found at unexpected place'.

]

{ #category : 'initialization' }
Scanner >> initScanner [

	typeTable := Scanner typeTable.
	isForTokenization := false.
	sentInLiterals := Set new.
	
]

{ #category : 'initialization' }
Scanner >> initScannerForTokenization [
	"Don't raise xIllegal when enocuntering an _"
	"Simpler implementation for Cuis"
	isForTokenization := true.
	
]

{ #category : 'multi-character scans' }
Scanner >> isAt: aChar [
	
	^ hereChar = aChar and: [aheadChar = aChar ifTrue: [self step. false] ifFalse: [true]]
]

{ #category : 'error handling' }
Scanner >> notify: string  [
	self error: string
]

{ #category : 'error handling' }
Scanner >> notify: string at: posiiton [
	"Parser compatible message"
	 
	^self notify: string 
]

{ #category : 'error handling' }
Scanner >> offEnd: aString  [
	"Parser overrides this"

	^self notify: aString
]

{ #category : 'multi-character scans' }
Scanner >> readUpToNext: aChar ifNotFound: aNotFoundBlock [

	self step.
	buffer reset.
	
	[self isAt: aChar]
		whileFalse: [
			buffer nextPut: self step.
			(hereChar = DoItCharacter and: [source atEnd]) ifTrue: [^aNotFoundBlock value ]].
	
	self step.
	token := buffer contents.
]

{ #category : 'initialization' }
Scanner >> scan: inputStream  [
	"Bind the input stream, fill the character buffers and first token buffer."

	source := inputStream.
	buffer := source collectionSpecies writeStream.
	self step.
	self step.
	self scanToken
]

{ #category : 'expression types' }
Scanner >> scanAllTokenPositionsInto: aBlock [
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments."

	| lastMark |
	lastMark := 1.
	[currentComment ifNotNil:
		[currentComment do:
			[:cmnt| | idx |
			 idx := source originalContents indexOfSubCollection: cmnt startingAt: lastMark.
			 (idx > 0 and: [idx < mark]) ifTrue:
				[aBlock value: idx - 1 value: (lastMark := idx + cmnt size)]].
		 currentComment := nil].
	mark ifNotNil:
		[(token == #- 
		  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue:
			[| savedMark |
			 savedMark := mark.
			 self scanToken.
			 token := token negated.
			 mark := savedMark].
		"Compensate for the fact that the parser uses two character lookahead.  Normally we must
		  remove the extra two characters.  But this mustn't happen for the last token at the end of stream."
		 aBlock
			value: mark
			value: self compensateTwoCharacterLookahead ].
	 (tokenType == #rightParenthesis
	  or: [tokenType == #doIt]) ifTrue:
		[^self].
	tokenType == #leftParenthesis
		ifTrue: 
			[self scanToken; scanAllTokenPositionsInto: aBlock]
		ifFalse: 
			[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
				ifTrue: 
					[self scanLitWord.
					 token == #true ifTrue: [token := true].
					 token == #false ifTrue: [token := false].
					 token == #nil ifTrue: [token := nil]]
				ifFalse:
					[(token == #- 
					  and: [(self typeTableAt: hereChar) == #xDigit])
						ifTrue: 
							[self scanToken.
							 token := token negated]]].
		self scanToken ] repeat
]

{ #category : 'public access' }
Scanner >> scanFieldNames: stringOrArray [
	"Answer an Array of Strings that are the identifiers in the input string, 
	stringOrArray. If passed an Array, just answer with that Array, i.e., 
	assume it has already been scanned."

	| strm |
	(stringOrArray isMemberOf: Array)
		ifTrue: [^stringOrArray].
	self scan: stringOrArray asString asPlainString readStream.
	strm := (Array new: 10) writeStream.
	[tokenType == #doIt]
		whileFalse: [
			tokenType == #word ifTrue: [strm nextPut: token].
			self scanToken ].
	^strm contents

	"Scanner new scanFieldNames: 'abc  def ghi' ('abc' 'def' 'ghi' )"
]

{ #category : 'expression types' }
Scanner >> scanForFindSelectorUpTo: terminator [
"
Scanner findSelectorTests
"
    | s |

    s := source collectionSpecies writeStream.
    [tokenType == terminator or: [tokenType == #doIt]] whileFalse: [
        tokenType caseOf: {
            [#leftParenthesis] -> [self scanToken; scanForFindSelectorUpTo: #rightParenthesis].
            [#leftBracket] -> [self scanToken; scanForFindSelectorUpTo: #rightBracket].
            [#leftBrace] -> [self scanToken; scanForFindSelectorUpTo: #rightBrace].
            [#keyword] -> [s nextPutAll: token].
        } otherwise: [].
        self scanToken
    ].
    ^s contents
]

{ #category : 'expression types' }
Scanner >> scanLitByteVec [
	"Also accept Floats besides bytes!
	#[1 2 3 255]
	#[1.0 0.2 1.0]
	#[1.0 -0.2e-23 1.0e4]
	"
	| stream |
	stream := nil.
	[ tokenType == #rightBracket or: [ tokenType == #doIt ] ] whileFalse: [
		(token == #- and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: [
			self scanToken.
			token := token negated ].
		((token isInteger and: [ token between: 0 and: 255 ]) or: [token isFloat])
			ifFalse: [ ^ self offEnd: '8-bit integer, floating point number, or right bracket expected' ].
		stream ifNil: [
			stream := ((token isFloat ifTrue: [Float64Array] ifFalse: [ByteArray]) new: 16) writeStream ].
		stream nextPut: token.
		self scanToken ].
	token := stream
		ifNotNil: [ stream contents ]
		ifNil: [
			"For back compatibility, if empty, assume ByteArray"
			ByteArray new ]
]

{ #category : 'expression types' }
Scanner >> scanLitVec [
	| s |
	s := WriteStream on: (Array new: 16).
	[tokenType == #rightParenthesis or: [tokenType == #doIt]] whileFalse:
		[tokenType == #leftParenthesis
			ifTrue: 
				[self scanToken; scanLitVec]
			ifFalse: 
				[(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon]])
					ifTrue: 
						[self scanLitWord.
						token == #true ifTrue: [token := true].
						token == #false ifTrue: [token := false].
						token == #nil ifTrue: [token := nil]]
					ifFalse:
						[(token == #- 
						  and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: 
							[self scanToken.
							 token := token negated]]].
		s nextPut: token.
		self scanToken].
	token := s contents
]

{ #category : 'expression types' }
Scanner >> scanLitWord [
	"Accumulate keywords and asSymbol the result."

	token := (source collectionSpecies streamContents: [ :stream |
		stream nextPutAll: token.
		[	| type |
			type := self typeTableAt: hereChar. 
			type == #xLetter or: [type == #xDiacritical] ] whileTrue: [
			self xLetter.
			stream nextPutAll: token ] ]) asSymbol
]

{ #category : 'public access' }
Scanner >> scanMessageParts: sourceString [
	"Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

	| coll nonKeywords |
	coll := OrderedCollection new.
	self scan: sourceString asPlainString readStream.
	nonKeywords := 0.
	[tokenType == #doIt] whileFalse:
		[(currentComment == nil or: [currentComment isEmpty])
			ifTrue: [coll addLast: nil]
			ifFalse: [coll addLast: currentComment removeFirst.
				[currentComment isEmpty] whileFalse:
					[coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
		(token numArgs < 1 or: [token = #| and: [ coll size > 1 ] ])
			ifTrue: [(nonKeywords := nonKeywords + 1) > 1 ifTrue: [^ coll]]
						"done with header"
			ifFalse: [nonKeywords := 0].
		coll addLast: token.
		self scanToken].
	(currentComment == nil or: [currentComment isEmpty])
		ifTrue: [coll addLast: nil]
		ifFalse: [coll addLast: currentComment removeFirst.
			[currentComment isEmpty] whileFalse: [
				coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
	^ coll
]

{ #category : 'expression types' }
Scanner >> scanToken [

	"Skip delimiters fast, there almost always is one."
	self skipDelimiters.

	mark := source position - 1.
	(tokenType at: 1) == $x "x as first letter"
		ifTrue: [self perform: tokenType "means perform to compute token & type"]
		ifFalse: [token := self step asSymbol "else just unique the first char"].
	^token
]

{ #category : 'public access' }
Scanner >> scanTokenPositionsIn: textOrString into: aBlock [
	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString."

	self initScannerForTokenization.
	source := textOrString asPlainString readStream.
	buffer := source collectionSpecies writeStream.
	self step.
	self step.
	self scanAllTokenPositionsInto: aBlock

	"| code |
	code := '       #( 1 2 #( 3 4 ))  16r123 123 123.0  ', (Scanner sourceCodeAt: #scanTokenPositionsIn:into:).
	Scanner new scanTokenPositionsIn: code into: [:start :end| Transcript cr; nextPut: $_; nextPutAll: (code copyFrom: start to: end); nextPut: $_; endEntry]"

	"CodeDiffBuilder buildDisplayPatchFrom:  (Scanner sourceCodeAt: #scanTokenPositionsIn:into:) to:  ((Scanner sourceCodeAt: #scanTokenPositionsIn:into:) copyReplaceAll: String crString with: '')"

	"CodeDiffBuilder buildDisplayPatchFrom:  'colorTable ^colorTable ifNil: [colorTable := ST80ColorTable]' to:'colorTable ^colorTable ifNil: [colorTable := ST80ColorTable]'"
]

{ #category : 'public access' }
Scanner >> scanTokens: textOrString  [
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimiters #( and ) in a 
	Smalltalk literal expression."

	self initScannerForTokenization.
	self scan: textOrString asPlainString readStream.
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"
]

{ #category : 'expression types - private' }
Scanner >> skipDelimiters [

	[(tokenType := self typeTableAt: hereChar) == #xDelimiter] whileTrue: [self step].  

]

{ #category : 'expression types' }
Scanner >> step [

	| c |
	c := hereChar.
	hereChar := aheadChar.
	source atEnd
		ifTrue: [aheadChar := DoItCharacter "doit"]
		ifFalse: [aheadChar := source next].
	^c
]

{ #category : 'multi-character scans' }
Scanner >> typeTableAt: aCharacterOrCodePoint [
	^self class typeTable: typeTable at: aCharacterOrCodePoint
]

{ #category : 'public access' }
Scanner >> typedScanTokens: textOrString  [
	"Answer an Array that has been tokenized with literals mapped to literals,
	 special characters mapped to symbols and variable names and keywords
	 to strings."
	| s |
	self initScannerForTokenization.
	self scan: textOrString asPlainString readStream.
	s := (Array new: 16) writeStream.
	[tokenType == #doIt] whileFalse: [
		(token == #- 
			and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: [
				self scanToken.
				token := token negated].
		s nextPut: token.
		self scanToken].
	^s contents

	"Scanner new typedScanTokens: (Scanner sourceCodeAt: #typedScanTokens:)"
]

{ #category : 'multi-character scans' }
Scanner >> xBacktick [

	token := $` asSymbol.
	tokenType := #backtick.
	self step.
]

{ #category : 'multi-character scans' }
Scanner >> xBinary [

	tokenType := #binary.
	token := source collectionSpecies streamContents: [ :stream |
	stream nextPut: self step.
	[	| type |
		type := self typeTableAt: hereChar.
		(type == #xBinary or: [ type == #xDiacritical ]) and: [
			hereChar ~~ $- or: [aheadChar isDigit not]]
	] whileTrue: [
		stream nextPut: self step]].
	token := token asSymbol.
]

{ #category : 'multi-character scans' }
Scanner >> xColon [
	"Allow := for assignment"
	
	aheadChar == $= ifTrue:
		[self step.
		tokenType := #leftArrow.
		self step.
		^ token := #':='].
	"Otherwise, just do what normal scan of colon would do"
	tokenType := #colon.
	^ token := self step asSymbol
]

{ #category : 'multi-character scans' }
Scanner >> xDelimiter [
	"ignore blanks etc."

	self scanToken
]

{ #category : 'multi-character scans' }
Scanner >> xDigit [
	"Form a number."

	tokenType := #number.
	(aheadChar = DoItCharacter and: [source atEnd
			and:  [source skipBack. source next ~= DoItCharacter]])
		ifTrue: [source skipBack "Read off the end last time"]
		ifFalse: [source skipBack; skipBack].
	token := [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step
]

{ #category : 'multi-character scans' }
Scanner >> xDollar [
	"form a Character literal"

	self step. "pass over $"
	token := self step.
	tokenType := #number "really should be Char, but rest of compiler doesn't know"
]

{ #category : 'multi-character scans' }
Scanner >> xDoubleQuote [
    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar := DoItCharacter.
    aStream := source collectionSpecies writeStream.
    self step.
    [hereChar == $"]
        whileFalse:
            [(hereChar = stopChar and: [source atEnd])
                ifTrue: [^self offEnd: self class unmatechedCommentQuoteErrorDescription].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment := OrderedCollection with: aStream contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken
]

{ #category : 'multi-character scans' }
Scanner >> xIllegal [
	"An illegal character was encountered"
	self notify: 'Illegal character (char code ' , hereChar printString, ' ', hereChar hex , ')' at: mark
]

{ #category : 'multi-character scans' }
Scanner >> xLetter [
	"Form a word or keyword."

	| type |
	buffer reset.
	[(type := self typeTableAt: hereChar) == #xLetter
		or: [type == #xDigit
		or: [type == #xUnderscore
		or: [type == #xDiacritical
		or: [ hereChar == $′ ]]]]  						"Also see #isValidInIdentifiers"
	] whileTrue: [
			"open code step for speed"
			buffer nextPut: hereChar.
			hereChar := aheadChar.
			aheadChar := source atEnd
				ifTrue: [DoItCharacter "doit"]
				ifFalse: [source next]].
	tokenType := (type == #colon or: [type == #xColon and: [aheadChar ~~ $=]])
		ifTrue: [
			buffer nextPut: self step.
			"Allow any number of embedded colons in literal symbols"
			[(self typeTableAt: hereChar) == #xColon]
				whileTrue:
					[buffer nextPut: self step].
			#keyword]
		ifFalse: [
			#word].
	token := buffer contents.
]

{ #category : 'multi-character scans' }
Scanner >> xLitQuote [
	"Symbols and vectors: #(1 (4 5) 2 3) #ifTrue:ifFalse: #'abc'."
	| start |
	start := mark.
	self step. "litQuote"
	self scanToken.
	tokenType == #leftParenthesis
		ifTrue: [
			self scanToken; scanLitVec.
			mark := start + 1.
			tokenType == #doIt
				ifTrue: [self offEnd: 'Unmatched parenthesis']]
		ifFalse: [
			tokenType == #leftBracket
				ifTrue: [
					self scanToken; scanLitByteVec.
					mark := start + 1.
					tokenType == #doIt
						ifTrue: [self offEnd: 'Unmatched bracket']]
				ifFalse: [
					(tokenType == #word or: [tokenType == #keyword or: [tokenType == #colon or: [tokenType == #leftArrow]]])
						ifTrue: [self scanLitWord]
						ifFalse: [tokenType == #string
							ifTrue: [token := token asSymbol]
							ifFalse: [
								(tokenType == #binary or: [ tokenType == #verticalBar ]) 
									ifFalse: [self notify: 'Invalid literal character' at: start + 1]]]]].
	mark := start.
	tokenType := #literal

	"#(Pen)
	#Pen
	#'Pen'
	##Pen
	###Pen
	"
]

{ #category : 'multi-character scans' }
Scanner >> xSingleQuote [
	
	"String."

	self readUpToNext: $' ifNotFound: [
		^self notify: 'Unmatched string quote' at: mark + 1].
	tokenType := #string
]

{ #category : 'multi-character scans' }
Scanner >> xUnderscore [
	"Figure out if x _foo (no space between _ and foo) 
	should be a selector or assignment"

	(#(xLetter xDigit xUnderscore xColon ) statePointsTo: (self typeTableAt: aheadChar))
		ifFalse: [
			self step.
			tokenType := #leftArrow.
			^token := #'_' ].
	^self xLetter.
]
