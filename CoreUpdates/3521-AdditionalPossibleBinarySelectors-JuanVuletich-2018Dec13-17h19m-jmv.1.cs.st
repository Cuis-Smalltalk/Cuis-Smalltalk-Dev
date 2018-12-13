'From Cuis 5.0 of 7 November 2016 [latest update: #3520] on 13 December 2018 at 5:20:43 pm'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 12/12/2018 16:22:24'!
xBinary

	tokenType _ #binary.
	token _ String streamContents: [ :stream |
	stream nextPut: self step.
	[	| type |
		type _ self typeTableAt: hereChar.
		type == #xBinary and: [hereChar ~= $- or: [aheadChar isDigit not]]
		] whileTrue: [
		stream nextPut: self step]].
	token _ token asSymbol! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 12/12/2018 15:33:25'!
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
	newTable atAll:  '¡!!%&+-*/\±×÷¬­¯,<=>”•–—˜™š›œ«»¿?@~€‚ƒŽ‘’“žŸ°·¢£¤¥µ¶„…§©®¹²³ªº' asByteArray put: #xBinary.

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
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!Parser methodsFor: 'private' stamp: 'jmv 12/12/2018 16:17:46'!
privateReadSelector
	| args selector |
	doitFlag := false.

	hereType == #word ifTrue: [
		^ here asSymbol ].

	self transformVerticalBarAndUpArrowIntoABinarySelector.

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

!Parser methodsFor: 'scanning' stamp: 'jmv 12/13/2018 17:07:02'!
transformVerticalBarAndUpArrowIntoABinarySelector
	"Transform a vertical bar and or a up arrow into a binary selector.
	Eventually aggregate a serie of immediately following vertical bars, up arrows and a binary selector.
	Note that this aggregation cannot occur at scan time, because a pair of vertical bars can be encountered in two valid constructs:
	- either as an empty temporaries specification,
	- or as a local temporaries specification in a block of arity > 0.
	Also do the same with some other special characters that are allowed as binary selectors, in addition to their traditional meaning"
	| special |
	special _ #(verticalBar upArrow).
	(special identityIncludes: hereType) ifFalse: [
		^ self ].
	here := here asString.
	hereType := #binary.
	[(special identityIncludes: tokenType) and: [hereMark + here size = mark]]
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

!Parser methodsFor: 'expression types' stamp: 'jmv 12/12/2018 16:17:30'!
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
			
			level >= 2 ifTrue: [self transformVerticalBarAndUpArrowIntoABinarySelector].
			((hereType == #binary )
				and: [level >= 2])
				ifTrue: 
					[start := self startOfNextToken.
					selector := self advance asSymbol.
					words := OrderedCollection with: (start  + requestorOffset to: self endOfLastToken + requestorOffset).
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
				sourceRange: (start to: self endOfLastToken)
				keywordsRanges: words.
	repeat]
		whileTrue: [].
	^true! !

!Parser methodsFor: 'expression types' stamp: 'jmv 12/12/2018 16:16:14'!
pattern: fromDoit inContext: ctxt
	" unarySelector | binarySelector arg | keyword arg {keyword arg} =>
	{selector, arguments, precedence}."
	
	doitFlag := fromDoit.
	
	fromDoit ifTrue: [^self doitPatternInContext: ctxt ].
	hereType == #word ifTrue: [^self unaryPattern ].
	self transformVerticalBarAndUpArrowIntoABinarySelector.
	hereType == #binary ifTrue: [^self binaryPattern ].
	hereType == #keyword ifTrue: [^self keywordPattern ]. 
	
	^self expected: 'Message pattern'
! !

!methodRemoval: Parser #transformVerticalBarAndUpArrowIntoABinarySelectorIfNecessary!
Parser removeSelector: #transformVerticalBarAndUpArrowIntoABinarySelectorIfNecessary!
