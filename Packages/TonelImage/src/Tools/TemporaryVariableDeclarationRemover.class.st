"
I remove declarations of a temporary variable from the children of a ParseNode.
"
Class {
	#name : 'TemporaryVariableDeclarationRemover',
	#superclass : 'ParseNodesDeclaringTemporaryVariableVisitor',
	#instVars : [
		'methodNode',
		'newSourceCode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
TemporaryVariableDeclarationRemover class >> in: aMethodNode for: aTemporaryVariable [

	| instance |

	instance := self for: aTemporaryVariable.
	instance methodNode: aMethodNode.
	^instance
]

{ #category : 'accessing' }
TemporaryVariableDeclarationRemover >> methodNode: aMethodNode [

	methodNode := aMethodNode.
]

{ #category : 'accessing' }
TemporaryVariableDeclarationRemover >> newSourceCode [

	^newSourceCode 
]

{ #category : 'source text generation' }
TemporaryVariableDeclarationRemover >> sourceTextWithoutMethodTemporaryDeclarationLine [
	
	| endTempsMark startTempsMark |
	
	startTempsMark := methodNode sourceText indexOf: $|.
	endTempsMark := methodNode sourceText indexOf: $| startingAt: startTempsMark + 1.
	^self sourceTextWithoutTemporaryDeclarationLineFrom: startTempsMark to: endTempsMark.
]

{ #category : 'source text generation' }
TemporaryVariableDeclarationRemover >> sourceTextWithoutTemporaryDeclaration [

	| temporaryVariablePositions variableDeclarationPosition | 

	temporaryVariablePositions := methodNode positionsForTemporaryVariable: temporaryVariable ifAbsent: nil.
	variableDeclarationPosition := {temporaryVariablePositions first}.
	^methodNode sourceText copyReplacing: variableDeclarationPosition with: ''
]

{ #category : 'source text generation' }
TemporaryVariableDeclarationRemover >> sourceTextWithoutTemporaryDeclarationLineFrom: firstIndex to: lastIndex [

	^methodNode sourceText copyReplaceFrom: firstIndex to: lastIndex with: ' '.
]

{ #category : 'source text generation' }
TemporaryVariableDeclarationRemover >> sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode [
	
	| sourceTextUpToEndTemps endTempsMark startTempsMark |
	endTempsMark := aBlockNode tempsMark.
	sourceTextUpToEndTemps := methodNode sourceText copyFrom: 1 to: endTempsMark - 1.
	startTempsMark := sourceTextUpToEndTemps findLast: [ :c | c = $| ].
	^self sourceTextWithoutTemporaryDeclarationLineFrom: startTempsMark to: endTempsMark
]

{ #category : 'source text generation' }
TemporaryVariableDeclarationRemover >> sourceTextWithoutTemporaryFromParseNode: aParseNode [
withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock
	
	^aParseNode temporaries size = 1 
		ifTrue: aTemporaryDeclarationLineRemovingBlock value
		ifFalse: [ self sourceTextWithoutTemporaryDeclaration ].
]

{ #category : 'visiting' }
TemporaryVariableDeclarationRemover >> visitBlockNodeDeclaringTemporary: aBlockNode [

	self 
		visitNodeDeclaringTemporary: aBlockNode 
		withTemporaryDeclarationLineRemover: [ 
			self sourceTextWithoutTemporaryDeclarationLineInBlockNode: aBlockNode ]
]

{ #category : 'visiting' }
TemporaryVariableDeclarationRemover >> visitMethodNodeDeclaringTemporary: aMethodNode [
	
	self 
		visitNodeDeclaringTemporary: aMethodNode 
		withTemporaryDeclarationLineRemover: [ self sourceTextWithoutMethodTemporaryDeclarationLine ]
]

{ #category : 'visiting' }
TemporaryVariableDeclarationRemover >> visitNodeDeclaringTemporary: aParseNode  [
	withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock

	newSourceCode := self
		sourceTextWithoutTemporaryFromParseNode: aParseNode
		withTemporaryDeclarationLineRemover: aTemporaryDeclarationLineRemovingBlock.
		
	methodNode methodClass compile: newSourceCode.
]
