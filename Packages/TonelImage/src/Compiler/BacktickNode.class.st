Class {
	#name : 'BacktickNode',
	#superclass : 'ParseNode',
	#instVars : [
		'sourceCode',
		'expression',
		'literalNode',
		'parser',
		'range'
	],
	#category : 'Compiler-ParseNodes',
	#package : 'Compiler',
	#tag : 'ParseNodes'
}

{ #category : 'assertions' }
BacktickNode class >> assertNodesIn: anExpression canBeReferencedInsideBacktickUsing: aParser startingAt: aPosition [

	^ anExpression nodesDo: [ :aNode |
		(self canNotBeReferencedInsideBacktick: aNode) ifTrue: [
			aParser
				notify: (self canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: aNode key)
				at: aPosition ]]
]

{ #category : 'assertions' }
BacktickNode class >> canNotBeReferencedInsideBacktick: aNode [

	^ aNode isSelfPseudoVariable
		or: [ aNode isSuperPseudoVariable
		or: [ aNode isThisContextPseudoVariable ]]
]

{ #category : 'assertions' }
BacktickNode class >> canNotReferencePseudoVarInsideBacktickErrorDescriptionFor: pseudoVariable [

	^ 'Can not reference ', pseudoVariable, ' inside backtick'
]

{ #category : 'instance creation' }
BacktickNode class >> expression: anExpression source: aSourceCode parser: aParser range: aRange [

	self assertNodesIn: anExpression canBeReferencedInsideBacktickUsing: aParser startingAt: aRange first.

	^self new initializeExpression: anExpression source: aSourceCode parser: aParser range: aRange 
]

{ #category : 'visiting' }
BacktickNode >> accept: aVisitor [

	^ aVisitor visitBacktickNode: self.
	
]

{ #category : 'initialization' }
BacktickNode >> addLiteralsOfExpressionToParser [

	expression nodesDo: [ :aNode |
		aNode isMessageNode ifTrue: [
			parser addToSentInLiterals: aNode selector key ].
		aNode isLiteralNode ifTrue: [
			(aNode literalValue isSymbol and: [ aNode literalValue ~= Scanner doItSelector ])
				ifTrue: [ parser addToSentInLiterals: aNode literalValue ]].
			 (aNode isLiteralVariableNode and: [ 
				aNode isReferencingBehavior or: [ aNode isReferencingGlobal ]])
					ifTrue: [ parser addToSentInLiterals: aNode variableBinding ]].
]

{ #category : 'code generation' }
BacktickNode >> analyseTempsWithin: scopeBlock rootNode: rootNode assignmentPools: assignmentPools [

	^self
]

{ #category : 'code generation' }
BacktickNode >> emitCodeForValue: aParseStack encoder: anEncoder [

	^self literalNode emitCodeForValue: aParseStack encoder: anEncoder 
]

{ #category : 'testing' }
BacktickNode >> equivalentTo: aParseNode [

	^ aParseNode isBacktickNode
		and: [ expression equivalentTo: aParseNode expression ]
]

{ #category : 'literal node' }
BacktickNode >> evaluateBacktickSourceCode [

	^[[[Compiler evaluate: sourceCode ]
		on: SyntaxErrorNotification
		do: [ :ex | parser notify: 'Can not compile: ', ex errorMessage at: range first ]]
		on: UndeclaredVariableReference
		do: [ :ex | parser notify: 'Can not compile: Variable ''', ex varName, ''' is not declared' at: range first ]]
		on: Error
		do: [ :ex | parser notify: 'Can not evaluate code: ', ex description at: range first ].
]

{ #category : 'expression' }
BacktickNode >> expression [

	^expression
]

{ #category : 'initialization' }
BacktickNode >> initializeExpression: anExpression source: aSourceCode parser: aParser range: aRange [

	expression := anExpression.
	sourceCode := aSourceCode.
	parser := aParser.
	range := aRange.

	self addLiteralsOfExpressionToParser.
]

{ #category : 'literal node' }
BacktickNode >> initializeLiteralNode [

	| backtickValue |

	backtickValue := self evaluateBacktickSourceCode.
	literalNode := parser encoder encodeLiteral: backtickValue range: range


]

{ #category : 'testing' }
BacktickNode >> isBacktickNode [

	^true
]

{ #category : 'literal node' }
BacktickNode >> literalNode [

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
	^literalNode
]

{ #category : 'printing' }
BacktickNode >> printOn: aStream indent: level [

	aStream nextPut: $`.
	expression printOn: aStream indent: level.
	aStream nextPut: $`.
]

{ #category : 'code generation' }
BacktickNode >> sizeCodeForValue: anEncoder [

	^self literalNode sizeCodeForValue: anEncoder 
]

{ #category : 'visiting' }
BacktickNode >> visitExpressionWith: aVisitor [

	expression accept: aVisitor
]

{ #category : 'visiting' }
BacktickNode >> visitLiteralWith: aVisitor [

	"Remember that creating the literal can signal an exception if the message sent in the expression
	has changed since the method was compiled - Hernan"
	self literalNode accept: aVisitor 
]
