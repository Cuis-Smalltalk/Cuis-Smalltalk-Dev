'From Cuis 5.0 [latest update: #3983] on 22 December 2019 at 7:34:47 pm'!

!classDefinition: #ArgumentDeclarationCounter category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #ArgumentDeclarationCounter
	instanceVariableNames: 'argumentName counter'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
Smalltalk renameClassNamed: #ParseNodesDeclaringTemporaryVariableVisitor as: #ParseNodesDeclaringTemporaryVariableVisitor!
Smalltalk renameClassNamed: #TemporaryVariableDeclarationCounter as: #TemporaryVariableDeclarationCounter!

!TemporaryVariableDeclarationRemover commentStamp: '<historical>' prior: 0!
I remove declarations of a temporary variable from the children of a ParseNode.!


!ArgumentDeclarationCounter methodsFor: 'initialization' stamp: 'EB 12/22/2019 19:14:35'!
initializeFor: anArgumentName

	argumentName := anArgumentName.
	counter := 0! !

!ArgumentDeclarationCounter methodsFor: 'visiting' stamp: 'EB 12/22/2019 19:21:08'!
declaresSoughtArgument: aParseNode

	^aParseNode arguments anySatisfy: [ :argument | argument name = argumentName ]! !

!ArgumentDeclarationCounter methodsFor: 'visiting' stamp: 'EB 12/22/2019 19:18:07'!
visitBlockNode: aBlockNode

	self visitPotentiallyDeclaringParseNode: aBlockNode.
	super visitBlockNode: aBlockNode! !

!ArgumentDeclarationCounter methodsFor: 'visiting' stamp: 'EB 12/22/2019 19:17:52'!
visitMethodNode: aMethodNode

	self visitPotentiallyDeclaringParseNode: aMethodNode.
	super visitMethodNode: aMethodNode! !

!ArgumentDeclarationCounter methodsFor: 'visiting' stamp: 'EB 12/22/2019 19:22:02'!
visitPotentiallyDeclaringParseNode: aParseNode

	(self declaresSoughtArgument: aParseNode) ifTrue: [ counter := counter + 1 ]! !

!ArgumentDeclarationCounter methodsFor: 'accessing' stamp: 'EB 12/22/2019 19:12:12'!
count

	^counter! !


!ArgumentDeclarationCounter class methodsFor: 'instance creation' stamp: 'EB 12/22/2019 19:14:12'!
for: anArgumentName

	^self new initializeFor: anArgumentName ! !


!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'EB 12/22/2019 18:41:19'!
isNodeDeclaringTemporary: aParseNode

	^aParseNode temporaries anySatisfy: [ :temporaryVariableNode |
		temporaryVariableNode key = temporaryVariable
	].! !


!SmalltalkEditor methodsFor: 'temp to inst var' stamp: 'EB 12/22/2019 19:31:10'!
temporaryToInstanceVariable

	self
		withNodeUnderCursorDo: [ :nodeUnderCursor |
			(nodeUnderCursor isTemp and: [nodeUnderCursor isArg not])
				ifTrue: [ TemporaryToInstanceVariableApplier on: self for: nodeUnderCursor name :: value ]
				ifFalse: [ morph flash ]]
		ifAbsent: [ morph flash ].! !


!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'EB 12/22/2019 18:48:54'!
assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName

	| counter |

	counter := TemporaryVariableDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: counter.

	counter count = 1 :: ifFalse: [
		self refactoringError: self inexistentTemporaryErrorDescription ].! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'EB 12/22/2019 19:32:35'!
assertNoOtherBlockIn: aMethodNode definesTemporaryNamed: aTemporaryVariableName

	| temporaryCounter argumentCounter |

	temporaryCounter := TemporaryVariableDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: temporaryCounter.

	argumentCounter := ArgumentDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: argumentCounter.

	temporaryCounter count + argumentCounter count > 1 ifTrue: [
		self refactoringError: self temporaryExistsInOtherBlockErrorDescription ].
	! !


!ArgumentDeclarationCounter reorganize!
('initialization' initializeFor:)
('visiting' declaresSoughtArgument: visitBlockNode: visitMethodNode: visitPotentiallyDeclaringParseNode:)
('accessing' count)
!


!ArgumentDeclarationCounter class reorganize!
('instance creation' for:)
!


!TemporaryToInstanceVariable class reorganize!
('instance creation' named:fromMethod:)
('error descriptions' inexistentTemporaryErrorDescription temporaryExistsAsInstVarInSubclassesErrorDescription temporaryExistsInOtherBlockErrorDescription temporaryExistsInOtherMethodsErrorDescription)
('assertions' assert:hasTemporaryNamed: assertNoOtherBlockIn:definesTemporaryNamed: assertNoOtherMethodIn:orSubclassesDefinesTemporaryNamed: assertSubclassesOf:haventGotInstanceVariableNamed:)
!
