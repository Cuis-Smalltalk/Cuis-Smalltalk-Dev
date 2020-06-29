'From Cuis 5.0 [latest update: #4241] on 27 June 2020 at 6:50:36 pm'!
!classDefinition: #Parser category: #'Compiler-Kernel'!
Scanner subclass: #Parser
	instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category advanced '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 18:41:23'!
literalExpression

	parseNode := self advanceWithRangeDo: [ :lexema :range | encoder encodeLiteral: lexema range: range ].

	^true! !

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 18:41:41'!
literalNumberExpression

	self advanceWithRangeDo: [ :minusChar :minusRange |
		self advanceWithRangeDo: [ :number :numberRange |
			parseNode := encoder encodeLiteral: number negated range: (minusRange first to: numberRange last)]].

	^true! !

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 18:47:00'!
parenthesisExpression

	self advance.
	self expression ifFalse: [^self expected: 'expression'].
	(self match: #rightParenthesis) ifFalse: [^self expected: 'right parenthesis'].

	^true! !

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 18:40:19'!
variableExpression

	parseNode _ self variable.

	"
	(parseNode isUndefTemp and: [self interactive] and: [
		((requestor isKindOf: Editor) or: [ requestor is: #Morph ])])
	"
	"If the requestor is of an exotic kind (like a telnet server) we might not be
	 allowed to open a PupUpMenu for querying the user"

	(parseNode isUndefTemp and: [ self interactive ])
		ifTrue: [self queryUndefined].
	parseNode nowHasRef.

	^true! !


!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 18:46:12'!
blockExpression
	"[ ({:var} |) (| {temps} |) (statements) ] => BlockNode."

	| blockNode tempsDeclarationNode variableNodes temporaryBlockVariables start |

	blockNode := BlockNode new.
	variableNodes := OrderedCollection new.
	start := prevMark + requestorOffset.
	"Gather parameters."
	[self match: #colon] whileTrue:
		[self argumentNameWithRangeDo: [ :argumentName :range |
			variableNodes addLast: (encoder bindBlockArg: argumentName within: blockNode range: range)]].
	(variableNodes size > 0 & (hereType ~~ #rightBracket) and: [(self match: #verticalBar) not]) ifTrue:
		[^self expected: 'Vertical bar'].

	tempsDeclarationNode := self temporaryBlockVariablesFor: blockNode.
	temporaryBlockVariables := tempsDeclarationNode allDeclaredVariableNodes.
	self statements: variableNodes innerBlock: true blockNode: blockNode.
	blockNode temporariesDeclaration: tempsDeclarationNode.

	(self match: #rightBracket) ifFalse: [^self expected: 'Period or right bracket'].

	blockNode noteSourceRangeStart: start end: self endOfLastToken encoder: encoder.

	"The scope of the parameters and temporary block variables is no longer active."
	temporaryBlockVariables do: [:variable | variable scope: -1].
	variableNodes do: [:variable | variable scope: -1].

	^true! !

!Parser methodsFor: 'backtick' stamp: 'HAW 6/27/2020 18:47:29'!
backtickExpression

	| start range |

	start := self startOfNextToken.
	self advance.
	self expression ifFalse: [^self expected: 'expression'].
	(self match: #backtick) ifFalse: [^self expected: 'backtick'].
	range := start to: prevEnd.

	parseNode := BacktickNode
		expression: parseNode
		source: (source contents copyFrom: range first+1 to: range last-1)
		parser: self
		range: range.

	encoder noteSourceRange: range forNode:  parseNode.

	^true! !

!Parser methodsFor: 'expression types' stamp: 'HAW 6/27/2020 19:05:25'!
	primaryExpression

		hereType == #word
			ifTrue: [
				parseNode _ self variable.

				"
				(parseNode isUndefTemp and: [self interactive] and: [
					((requestor isKindOf: Editor) or: [ requestor is: #Morph ])])
				"
				"If the requestor is of an exotic kind (like a telnet server) we might not be
				 allowed to open a PupUpMenu for querying the user"

				(parseNode isUndefTemp and: [ self interactive ])
					ifTrue: [self queryUndefined].
				parseNode nowHasRef.
				^ true].
		hereType == #leftBracket
			ifTrue: [
				advanced := true.
				self advance.
				self blockExpression.
				^true].
		hereType == #leftBrace
			ifTrue: [
				self braceExpression.
				^true].
		hereType == #leftParenthesis
			ifTrue: [
				self advance.
				self expression ifFalse: [^self expected: 'expression'].
				(self match: #rightParenthesis)
					ifFalse: [^self expected: 'right parenthesis'].
				^true].
		(hereType == #string or: [hereType == #number or: [hereType == #literal]])
			ifTrue: [
				parseNode := self advanceWithRangeDo: [ :lexema :range | encoder encodeLiteral: lexema range: range ].
				^true].
		(here == #- and: [tokenType == #number])
			ifTrue: [
				self advanceWithRangeDo: [ :minusChar :minusRange |
					self advanceWithRangeDo: [ :number :numberRange |
						parseNode := encoder encodeLiteral: number negated range: (minusRange first to: numberRange last)]].
				^true].

		hereType == #backtick
			ifTrue: [
				self backtickExpression.
				^true ].

		^false! !

!classDefinition: #Parser category: #'Compiler-Kernel'!
	Scanner subclass: #Parser
		instanceVariableNames: 'here hereType hereMark hereEnd prevMark prevEnd encoder requestor parseNode failBlock requestorOffset tempsMark doitFlag properties category advanced'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Compiler-Kernel'!

!Parser reorganize!
('private' addComment init:notifying:failBlock: initPattern:notifying:return: privateReadSelector privateReadSelectorFrom:)
('pragmas' addPragma: pragmaLiteral: pragmaPrimitives pragmaSequence pragmaStatement properties)
('error handling' addWarning: expected: fail interactive notify: notify:at: offEnd:)
('scanning' advance advanceWithRangeDo: compensateTwoCharacterLookahead endOfLastToken match: matchReturn matchToken: startOfNextToken transformVerticalBarAndUpArrowIntoABinarySelector)
('primitives' allocateLiteral: externalFunctionDeclaration externalType: primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:)
('expression types' addKeywordPatternPartTo:keywordRanges:arguments: argumentName argumentNameWithRangeDo: assignment: binaryPattern blockExpression braceExpression cascade chain createCascadeNodeWith:and: doitPatternInContext: expression keywordPattern literalExpression literalNumberExpression messagePart:repeat: method:doIt:context: newMethodNode parenthesisExpression parseBlock pattern:inContext: primaryExpression statements:innerBlock: statements:innerBlock:blockNode: temporaries temporaryBlockVariablesFor: unaryPattern variable variableExpression)
('temps' bindArg: bindArg:range: bindTemp: createTempDeclarationOf:sourceRange: createTempsDeclarationWith:sourceRange: noTempsDeclaration)
('error correction' canDeclareClassVariable canDeclareInstanceVariable correctSelector:wordIntervals:exprInterval:ifAbort: correctVariable:interval: createEmptyTempsDeclarationAfter: createEmptyTempsDeclarationIfNecessaryIn: createTempDeclarationInMethodWith: declarationRangesForTemps: declareClassVar: declareGlobal: declareInstVar: declareTemp:at: declareTempAndPaste: declareUndeclaredTemps: declareUndeclaredTemps:inBlock: defineClass: filterSeparatorsToTheLeftOn:startingAt: filterSeparatorsToTheRightOn:startingAt: hasNoTempDeclarationPipes: insertWord:at: isLastPipeOfEmptyTempsDeclaration: newRangeRemovingTempsDeclarationFrom:startingAt:to: pasteTemp:before: pasteTempAtMethodLevel: possibleVariablesFor: queryUndefined rangesForRemovableUnusedTempsInBlockNode: rangesForRemovableUnusedTempsOf: removeUnusedTempRange:with: removeUnusedTempsOf: removeUnusedTempsRanges: selectRemovableUnusedTempsFrom: sourceDelta substituteSelector:wordIntervals: substituteVariable:atInterval: substituteWord:wordInterval:offset: unusedTempsOf:)
('public access' encoder encoderClass: parse:class: parse:class:category:noPattern:doIt:context:notifying:ifFail: parse:class:noPattern: parse:class:noPattern:context:notifying:ifFail: parse:class:noPattern:notifying:ifFail: parseSelector:)
('interactive checks' performInteractiveChecks:)
('backtick' addToSentInLiterals: backtickExpression)
!
