'From Cuis 4.2 of 25 July 2013 [latest update: #2927] on 4 September 2016 at 10:54:16 pm'!

!Parser methodsFor: 'scanning' stamp: 'jmv 9/4/2016 16:36:07'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0"
	here := here asString.
	hereType := #binary.
	[(tokenType == #verticalBar or: [tokenType == #upArrow]) and: [hereMark + here size = mark]]
		whileTrue: [
			here := here , token asString.
			hereEnd := hereEnd + 1.
			self scanToken].
	(tokenType == #binary and: [hereMark + here size = mark])
		ifTrue: [
			here := here asString , token.
			hereType := #binary.
			hereEnd := hereEnd + token size.
			self scanToken].! !


!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 9/4/2016 22:48:40'!
isBinarySelectorCharacter: aCharacter

	aCharacter isValidInIdentifiers ifTrue: [^false].
	aCharacter isSeparator ifTrue: [^false].

	('"#$'':().;[]{}_'  includes: aCharacter) 
		ifTrue:[^false].
	aCharacter asciiValue = Scanner doItCharacterValue ifTrue: [^false "the doIt char"].
	aCharacter asciiValue = 0 ifTrue: [^false].
	"Any other char is ok as a binary selector char."
	^true
! !


!String class methodsFor: 'initialization' stamp: 'jmv 9/4/2016 17:16:24'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase |

	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) asciiValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first asciiValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second asciiValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	CaseInsensitiveOrder _ newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	#(oplus otimes odot dot circle bullet) do: [:each|
		newOrder at: (Character perform: each) asciiValue + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first asciiValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase asciiValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character allCharacters collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character allCharacters collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character allCharacters collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 9/4/2016 16:20:01'!
xBinary

	tokenType _ #binary.
	token _ String streamContents: [ :stream |
	stream nextPut: self step.
	[	| type |
		type _ self typeTableAt: hereChar.
		(#(xBinary verticalBar upArrow) identityIncludes: type)
			and: [hereChar ~= $- or: [aheadChar isDigit not]]
		] whileTrue: [
		stream nextPut: self step]].
	token _ token asSymbol! !


!Parser methodsFor: 'private' stamp: 'jmv 9/4/2016 17:12:32'!
privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	(hereType == #verticalBar or: [hereType == #upArrow]) ifTrue: [
		self transformVerticalBarAndUpArrowIntoABinarySelector].

	hereType == #binary ifTrue: [
		^ here asSymbol ].

	hereType == #keyword ifTrue: [
		selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue: [
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ selector contents asSymbol ].

	^self expected: 'Message pattern'! !

!Parser methodsFor: 'expression types' stamp: 'jmv 9/4/2016 15:52:53'!
messagePart: level repeat: repeat

	| start receiver selector args precedence words keywordStart |
	[receiver := parseNode.
	(hereType == #keyword and: [level >= 3])
		ifTrue: 
			[start := self startOfNextToken.
			selector := WriteStream on: (String new: 32).
			args := OrderedCollection new.
			words := OrderedCollection new.
			[hereType == #keyword]
				whileTrue: 
					[keywordStart := self startOfNextToken + requestorOffset.
					selector nextPutAll: self advance.
					words addLast: (keywordStart to: self endOfLastToken + requestorOffset).
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 2 repeat: true.
					args addLast: parseNode].
			(Symbol hasInterned: selector contents ifTrue: [ :sym | selector := sym])
				ifFalse: [ selector := self correctSelector: selector contents
										wordIntervals: words
										exprInterval: (start to: self endOfLastToken)
										ifAbort: [ ^ self fail ] ].
			precedence := 3]
		ifFalse: [
			
			(level >= 2 and: [hereType == #verticalBar or: [hereType == #upArrow]]) ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					self primaryExpression ifFalse: [^self expected: 'Argument'].
					self messagePart: 1 repeat: true.
					args := Array with: parseNode.
					precedence := 2]
				ifFalse: [hereType == #word
						ifTrue: 
							[start := self startOfNextToken.
							selector := self advance.
							args := #().
							words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
							(Symbol hasInterned: selector ifTrue: [ :sym | selector := sym])
								ifFalse: [ selector := self correctSelector: selector
													wordIntervals: words
													exprInterval: (start to: self endOfLastToken)
													ifAbort: [ ^ self fail ] ].
							precedence := 1]
						ifFalse: [^args notNil]]].
	parseNode := MessageNode new
				receiver: receiver
				selector: selector
				arguments: args
				precedence: precedence
				from: encoder
				sourceRange: (start to: self endOfLastToken).
	repeat]
		whileTrue: [].
	^true! !

!Parser methodsFor: 'expression types' stamp: 'jmv 9/4/2016 16:52:11'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	| args selector |
	doitFlag := fromDoit.
	fromDoit ifTrue:
		[^ctxt == nil
			ifTrue: [{#DoIt. {}. 1}]
			ifFalse: [{#DoItIn:. {encoder encodeVariable: encoder doItInContextName}. 3}]].

	hereType == #word ifTrue: [^ {self advance asSymbol. {}. 1}].


	(hereType == #verticalBar
			or: [hereType == #upArrow])
		ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].

	hereType == #binary ifTrue: [
		selector := self advance asSymbol.
		args := Array with: (encoder bindArg: self argumentName).
		^ {selector. args. 2}].

	hereType == #keyword ifTrue: [
		selector := WriteStream on: (String new: 32).
		args := OrderedCollection new.
		[hereType == #keyword] whileTrue:[
			selector nextPutAll: self advance.
			args addLast: (encoder bindArg: self argumentName).
		].
		^ {selector contents asSymbol. args. 3}].
	^self expected: 'Message pattern'! !


!SHParserST80 methodsFor: 'scan' stamp: 'jmv 9/4/2016 22:49:03'!
scanBinary
	| c d |
	c := self currentChar.
	currentTokenSourcePosition := sourcePosition.
	currentToken := c asString.
	d := self nextChar.
	((self isBinarySelectorCharacter: c) or: [c == $:]) ifFalse: [^currentToken].
	(c == $: and: [d == $=]) 
		ifTrue: [" := assignment"
			currentToken := currentToken , d asString.
			self nextChar.
			^currentToken].
	c _ d.
	[
		d _ self peekChar.
		c == $-
			ifTrue: [ d isDigit not ]
			ifFalse: [ self isBinarySelectorCharacter: c ]
	]
		whileTrue: [
			currentToken _ currentToken copyWith: c.
			c _ self nextChar ].
	^currentToken! !

!SHParserST80 methodsFor: 'scan' stamp: 'jmv 9/4/2016 22:48:55'!
scanIdentifier
	| c start |
	start _ sourcePosition.
	[ (c _ self nextChar) isValidInIdentifiers ] whileTrue: [].
	(c == $: and: [(self isBinarySelectorCharacter: self peekChar) not]) 
		ifTrue: [self nextChar].
	currentToken _ source copyFrom: start to: sourcePosition - 1.
	currentTokenSourcePosition _ start! !

!SHParserST80 methodsFor: 'token testing' stamp: 'jmv 9/4/2016 22:48:45'!
isBinary
	| c |
	(currentToken isNil or: [self isName or: [self isKeyword]]) 
		ifTrue: [^false].
	1 to: currentToken size do: [ :i |
		c := currentToken at: i.
		(self isBinarySelectorCharacter: c)
			ifFalse: [^false]].
	^true! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/4/2016 22:48:48'!
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
	(self isBinarySelectorCharacter: c) ifTrue: [
		self parseSymbolSelector.
		^self ].
	( c isValidStartOfIdentifiers or: [c == $:]) ifTrue: [
		self parseSymbolIdentifier.
		^self].
	self parseCharSymbol! !

!SHParserST80 methodsFor: 'parse' stamp: 'jmv 9/4/2016 22:48:51'!
parseSymbolSelector
	| start end |
	start := sourcePosition - 1.
	end := sourcePosition.
	[self isBinarySelectorCharacter: self nextChar] 
		whileTrue: [end := sourcePosition].
	self scanPast: #symbol start: start end: end! !

!methodRemoval: SHParserST80 #isSelectorCharacter:!
SHParserST80 removeSelector: #isSelectorCharacter:!
String initialize!
