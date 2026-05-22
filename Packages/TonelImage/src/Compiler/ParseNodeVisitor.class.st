"
I am an abstract superclass for ParseNode visitors that functions as a null visitor.  Here's the code that defines my interface:

(SystemNavigation default allImplementorsOf: #accept: localTo: ParseNode) do:
	[:methodReference|
	methodReference compiledMethod messages do:
		[:sel|
		((sel beginsWith: 'visit')
		and: [sel numArgs = 1]) ifTrue:
			[ParseNodeVisitor
				compile: (String streamContents:
							[:str|
							str nextPutAll: sel;
								space;
								nextPut: $a.
							methodReference classSymbol first isVowel ifTrue:
								[str nextPut: $n].
							str nextPutAll: methodReference classSymbol])
				classified: 'visiting']]]
"
Class {
	#name : 'ParseNodeVisitor',
	#superclass : 'Object',
	#category : 'Compiler-Support',
	#package : 'Compiler',
	#tag : 'Support'
}

{ #category : 'visiting' }
ParseNodeVisitor >> visitAssignmentNode: anAssignmentNode [
	"N.B.  since assigment happens after the value is evaluated the value is visited first."
	anAssignmentNode value accept: self.
	anAssignmentNode variable accept: self
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitBacktickNode: aBacktickNode [

	"By default only visits the expresion of the backtick and not its literal.
	If visiting the literal is necessary, you can use the message visitLiteralWith: but
	remember that the litercal can be nil if the expresion was not evaluated - Hernan"

	aBacktickNode visitExpressionWith: self.
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitBlockNode: aBlockNode [

	self visitIfNotNil: aBlockNode temporariesDeclaration.
	aBlockNode statementsDo:
		[ :statement| statement accept: self ]
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitBraceNode: aBraceNode [

	aBraceNode elementsDo:
		[ :element | element accept: self ]
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitCascadeNode: aCascadeNode [
	aCascadeNode receiver accept: self.
	aCascadeNode messages do:
		[:message| self visitMessageNodeInCascade: message]
]

{ #category : 'private - visiting' }
ParseNodeVisitor >> visitIfNotNil: aParseNode [
	"RNG: this was implemented to support unexpected nil temporary declarations
	(coming from instances of MethodNode and BlockNode that were living in the image
	before the new parse nodes were introduced)"

	aParseNode ifNotNil: [ aParseNode accept: self ]
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitInstanceVariableNode: anInstanceVariableNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitLiteralNode: aLiteralNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitLiteralVariableNode: aLiteralVariableNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitMessageNode: aMessageNode [

	aMessageNode receiver accept: self.
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do: [:argument| argument accept: self]
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitMessageNodeInCascade: aMessageNode [
	"receiver is nil for cascades"
	aMessageNode selector accept: self.
	aMessageNode argumentsInEvaluationOrder do:
		[:argument| argument accept: self]
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitMethodNode: aMethodNode [

	self visitIfNotNil: aMethodNode temporariesDeclaration.
	aMethodNode block accept: self
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitNewArrayNode: aNewArrayNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitRemoteTempVectorNode: aRemoteTempVectorNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitReturnNode: aReturnNode [
	aReturnNode expr accept: self
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitSelectorNode: aSelectorNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitTempVariableNode: aTempVariableNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitTemporariesDeclarationNode: aTemporariesDeclarationNode [

	aTemporariesDeclarationNode temporaryDeclarationNodesDo:
		[ :temporaryDeclarationNode | temporaryDeclarationNode accept: self ]
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitTemporaryDeclarationNode: aTemporaryDeclarationNode [

	^aTemporaryDeclarationNode variableNode accept: self
]

{ #category : 'visiting' }
ParseNodeVisitor >> visitUndeclaredVariableNode: aVariableNode [

]

{ #category : 'visiting' }
ParseNodeVisitor >> visitVariableNode: aVariableNode [

]
