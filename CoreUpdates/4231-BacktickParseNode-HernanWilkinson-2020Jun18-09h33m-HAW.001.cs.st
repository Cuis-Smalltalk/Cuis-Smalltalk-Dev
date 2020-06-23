'From Cuis 5.0 [latest update: #4226] on 22 June 2020 at 4:18:36 pm'!
!classDefinition: #BacktickNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #BacktickNode
	instanceVariableNames: 'sourceCode expression literalNode parser range'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!

!Parser methodsFor: 'as yet unclassified' stamp: 'HAW 6/22/2020 16:05:36'!
addToSentInLiterals: aSymbol

	sentInLiterals add: aSymbol ! !

!Parser methodsFor: 'as yet unclassified' stamp: 'HAW 6/22/2020 16:15:27'!
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

	encoder noteSourceRange: range forNode:  parseNode.! !


!BacktickNode methodsFor: 'initialization' stamp: 'HAW 6/22/2020 16:07:07'!
addSymbolsOfExpressionToParser

	expression nodesDo: [ :aNode |
		aNode isMessageNode ifTrue: [ parser addToSentInLiterals: aNode selector key ].
		aNode isLiteralNode ifTrue: [ (aNode literalValue isSymbol and: [ aNode literalValue ~= Scanner doItSelector ])
			ifTrue: [ parser addToSentInLiterals: aNode literalValue ]]]! !

!BacktickNode methodsFor: 'initialization' stamp: 'HAW 6/22/2020 16:06:52'!
initializeExpression: anExpression source: aSourceCode parser: aParser range: aRange

	expression := anExpression.
	sourceCode := aSourceCode.
	parser := aParser.
	range := aRange.

	self addSymbolsOfExpressionToParser.! !

!BacktickNode methodsFor: 'literal node' stamp: 'HAW 6/21/2020 17:09:55'!
evaluateBacktickSourceCode

	^[[[Compiler evaluate: sourceCode ]
		on: SyntaxErrorNotification
		do: [ :ex | parser notify: 'Can not compile: ', ex errorMessage at: range first ]]
		on: UndeclaredVariableReference
		do: [ :ex | parser notify: 'Can not compile: Variable ''', ex varName, ''' is not declared' at: range first ]]
		on: Error
		do: [ :ex | parser notify: 'Can not evaluate code: ', ex description at: range first ].! !

!BacktickNode methodsFor: 'literal node' stamp: 'HAW 6/21/2020 17:09:36'!
initializeLiteralNode

	| backtickValue |

	backtickValue := self evaluateBacktickSourceCode.
	literalNode := parser encoder encodeLiteral: backtickValue range: range

! !

!BacktickNode methodsFor: 'literal node' stamp: 'HAW 6/22/2020 15:22:48'!
literalNode

	"The literalNode is initialize if really needed. There are situations when only the method node of a compiled method is needed
	and therefore the literal generated for the backtick is not necessary.
	The literal is needed when compiling the source code and sadly, when debugging because the debugger needs to regenerate
	the code for the temp bindings and the relationship between the bytecodes and the source ranges of the nodes.

	If some message of the backtick source code has changed since the time the method was compiled, the resulting literal
	could be different to the one generated when the method was originally compiled.
	The change could include signaling an exception. That could happen while debuggin. That would mean that the backtick
	code is not compliant with what currently should happen, so although it is a bothering solution, it is the more correct one
	since it shows the difference - Hernan"

	literalNode ifNil: [ self initializeLiteralNode ].
	^literalNode! !

!BacktickNode methodsFor: 'visiting' stamp: 'HAW 6/21/2020 13:20:00'!
accept: aVisitor

	aVisitor visitBacktickNode: self.
	! !

!BacktickNode methodsFor: 'visiting' stamp: 'HAW 6/21/2020 13:25:36'!
visitExpressionWith: aVisitor

	expression accept: aVisitor! !

!BacktickNode methodsFor: 'visiting' stamp: 'HAW 6/22/2020 15:13:14'!
visitLiteralWith: aVisitor

	"Remember that creating the literal can signal an exception if the message sent in the expression
	has changed since the method was compiled - Hernan"
	self literalNode accept: aVisitor ! !

!BacktickNode methodsFor: 'printing' stamp: 'HAW 6/22/2020 10:34:24'!
printOn: aStream indent: level

	aStream nextPut: $`.
	expression printOn: aStream indent: level.
	aStream nextPut: $`.! !

!BacktickNode methodsFor: 'code generation' stamp: 'HAW 6/21/2020 13:27:32'!
analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools

	^self! !

!BacktickNode methodsFor: 'code generation' stamp: 'HAW 6/21/2020 17:10:23'!
emitCodeForValue: aParseStack encoder: anEncoder

	^self literalNode emitCodeForValue: aParseStack encoder: anEncoder ! !

!BacktickNode methodsFor: 'code generation' stamp: 'HAW 6/21/2020 17:10:44'!
sizeCodeForValue: anEncoder

	^self literalNode sizeCodeForValue: anEncoder ! !


!BacktickNode class methodsFor: 'instance creation' stamp: 'HAW 6/22/2020 16:07:16'!
expression: anExpression source: aSourceCode parser: aParser range: aRange

	self assertNodesIn: anExpression canBeReferencedInsideBacktickUsing: aParser startingAt: aRange first.

	^self new initializeExpression: anExpression source: aSourceCode parser: aParser range: aRange ! !

!BacktickNode class methodsFor: 'assertions' stamp: 'HAW 6/22/2020 16:03:49'!
assertNodesIn: anExpression canBeReferencedInsideBacktickUsing: aParser startingAt: aPosition

	^ anExpression nodesDo: [ :aNode |
		(self canNotBeReferencedInsideBacktick: aNode) ifTrue: [
			aParser
				notify: (self canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: aNode key)
				at: aPosition ]]! !

!BacktickNode class methodsFor: 'assertions' stamp: 'HAW 6/22/2020 15:59:58'!
canNotBeReferencedInsideBacktick: aNode

	^ aNode isSelfPseudoVariable
		or: [ aNode isSuperPseudoVariable
		or: [ aNode isThisContextPseudoVariable ]]! !

!BacktickNode class methodsFor: 'assertions' stamp: 'HAW 6/22/2020 15:59:41'!
canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: pseudoVariable

	^ 'Can not reference ', pseudoVariable, ' inside backtick'! !


!ParseNodeVisitor methodsFor: 'visiting' stamp: 'HAW 6/22/2020 15:11:41'!
visitBacktickNode: aBacktickNode

	"By default only visits the expresion of the backtick and not its literal.
	If visiting the literal is necessary, you can use the message visitLiteralWith: but
	remember that the litercal can be nil if the expresion was not evaluated - Hernan"

	aBacktickNode visitExpressionWith: self.! !


!ParseNodeEnumerator methodsFor: 'visiting' stamp: 'HAW 6/21/2020 12:43:54'!
visitBacktickNode: aBacktickNode

	(theSelectBlock isNil or: [theSelectBlock value: aBacktickNode]) ifFalse:
		[^nil].
	theBlock value: aBacktickNode.
	^super visitBacktickNode: aBacktickNode! !


!Compiler methodsFor: 'private' stamp: 'HAW 6/22/2020 16:10:03'!
translate: aStream noPattern: noPattern doIt: doIt ifFail: failBlock

	^self parser
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		doIt: doIt
		context: context
		notifying: requestor
		ifFail: [^failBlock value]! !


!Scanner methodsFor: 'expression types - private' stamp: 'HAW 8/15/2018 19:53:35'!
skipDelimitersAndBacktickIfNecessary

	[self skipDelimiters.
	self isBacktickAndShouldIgnoreIt ] whileTrue: [self step].  ! !

!Scanner methodsFor: 'expression types' stamp: 'HAW 6/22/2020 16:11:44'!
scanToken

	"Skip delimiters fast, there almost always is one."
	self skipDelimiters.

	mark := source position - 1.
	(tokenType at: 1) = $x "x as first letter"
		ifTrue: [self perform: tokenType "means perform to compute token & type"]
		ifFalse: [token := self step asSymbol "else just unique the first char"].
	^token! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 6/22/2020 16:08:32'!
xBacktick

	token := $` asSymbol.
	tokenType := #backtick.
	self step.! !

!Scanner methodsFor: 'initialization' stamp: 'HAW 6/22/2020 16:10:19'!
initScanner

	buffer := WriteStream on: (String new: 40).
	typeTable := Scanner typeTable.
	isForTokenization := false.
	sentInLiterals := Set new.
	! !

!Scanner methodsFor: 'initialization' stamp: 'HAW 6/22/2020 16:10:23'!
initScannerForTokenization
	"Don't raise xIllegal when enocuntering an _"
	"Simpler implementation for Cuis"
	isForTokenization _ true.
	! !


!Parser methodsFor: 'expression types' stamp: 'HAW 6/22/2020 16:16:06'!
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

!methodRemoval: BacktickNode class #addSymbolsOf:to: stamp: 'HAW 6/22/2020 16:07:18'!
BacktickNode class removeSelector: #addSymbolsOf:to:!
!methodRemoval: BacktickNode class #of:source: stamp: 'HAW 6/22/2020 15:08:22'!
BacktickNode class removeSelector: #of:source:!
!methodRemoval: BacktickNode class #expression:literal: stamp: 'HAW 6/22/2020 15:08:17'!
BacktickNode class removeSelector: #expression:literal:!
!methodRemoval: BacktickNode class #expression:source:encoder:range: stamp: 'HAW 6/21/2020 17:08:28'!
BacktickNode class removeSelector: #expression:source:encoder:range:!
!methodRemoval: BacktickNode class #assertNodesIn:canBeReferencedInsideBacktickStartingAt: stamp: 'HAW 6/22/2020 16:02:22'!
BacktickNode class removeSelector: #assertNodesIn:canBeReferencedInsideBacktickStartingAt:!
!methodRemoval: BacktickNode class #of: stamp: 'HAW 6/22/2020 15:07:35'!
BacktickNode class removeSelector: #of:!
!methodRemoval: BacktickNode #initializeExpression:literal: stamp: 'HAW 6/22/2020 15:09:09'!
BacktickNode removeSelector: #initializeExpression:literal:!
!methodRemoval: BacktickNode #addSymbolsOf:to: stamp: 'HAW 6/22/2020 16:07:10'!
BacktickNode removeSelector: #addSymbolsOf:to:!
!methodRemoval: BacktickNode #initializeLiteral stamp: 'HAW 6/21/2020 13:05:36'!
BacktickNode removeSelector: #initializeLiteral!
!methodRemoval: BacktickNode #initializeOf: stamp: 'HAW 6/21/2020 13:05:40'!
BacktickNode removeSelector: #initializeOf:!
!methodRemoval: BacktickNode #literal stamp: 'HAW 6/21/2020 13:04:30'!
BacktickNode removeSelector: #literal!
!methodRemoval: BacktickNode #assertWasCompiledNotIgnoringBacktick stamp: 'HAW 6/22/2020 15:13:49'!
BacktickNode removeSelector: #assertWasCompiledNotIgnoringBacktick!
!methodRemoval: BacktickNode #expression stamp: 'HAW 6/21/2020 13:20:17'!
BacktickNode removeSelector: #expression!
!methodRemoval: BacktickNode #initializeOf:source: stamp: 'HAW 6/22/2020 15:09:12'!
BacktickNode removeSelector: #initializeOf:source:!
!methodRemoval: BacktickNode #initializeExpression:source:encoder:range: stamp: 'HAW 6/21/2020 17:08:55'!
BacktickNode removeSelector: #initializeExpression:source:encoder:range:!
!methodRemoval: Parser class #canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: stamp: 'HAW 6/22/2020 15:59:35'!
Parser class removeSelector: #canNotReferencePseudoVarInsideBacktickErrorDescriptionFor:!
!methodRemoval: Parser #compileBacktickCodeHandlingErrors: stamp: 'HAW 6/21/2020 16:21:31'!
Parser removeSelector: #compileBacktickCodeHandlingErrors:!
!methodRemoval: Parser #canNotBeReferencedInsideBacktick: stamp: 'HAW 6/22/2020 15:59:52'!
Parser removeSelector: #canNotBeReferencedInsideBacktick:!
!methodRemoval: Parser #compileBacktickCodeHandlingErrors:at: stamp: 'HAW 6/21/2020 16:22:03'!
Parser removeSelector: #compileBacktickCodeHandlingErrors:at:!
!methodRemoval: Parser #evaluateBacktickCode:handlingErrorsAt: stamp: 'HAW 6/22/2020 16:14:48'!
Parser removeSelector: #evaluateBacktickCode:handlingErrorsAt:!
!methodRemoval: Scanner #compileBacktickCodeHandlingErrors stamp: 'HAW 6/22/2020 16:08:26'!
Scanner removeSelector: #compileBacktickCodeHandlingErrors!
!methodRemoval: Scanner #compileBacktickCode: stamp: 'HAW 6/22/2020 16:10:43'!
Scanner removeSelector: #compileBacktickCode:!
!methodRemoval: Scanner #compileBacktickCode stamp: 'HAW 6/22/2020 16:08:23'!
Scanner removeSelector: #compileBacktickCode!

!BacktickNode reorganize!
('initialization' addSymbolsOfExpressionToParser initializeExpression:source:parser:range:)
('literal node' evaluateBacktickSourceCode initializeLiteralNode literalNode)
('visiting' accept: visitExpressionWith: visitLiteralWith:)
('printing' printOn:indent:)
('code generation' analyseTempsWithin:rootNode:assignmentPools: emitCodeForValue:encoder: sizeCodeForValue:)
('*LiveTyping' typesIn:addingIncompleteTypeInfoTo:)
!


!BacktickNode class reorganize!
('instance creation' expression:source:parser:range:)
('assertions' assertNodesIn:canBeReferencedInsideBacktickUsing:startingAt: canNotBeReferencedInsideBacktick: canNotReferencePseudoVarInsideBacktickErrorDescriptionFor:)
!
