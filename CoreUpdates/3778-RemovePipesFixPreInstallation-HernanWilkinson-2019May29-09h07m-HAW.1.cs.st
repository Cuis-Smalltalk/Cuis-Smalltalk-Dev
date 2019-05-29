'From Cuis 5.0 of 7 November 2016 [latest update: #3777] on 29 May 2019 at 9:08:47 am'!
!classDefinition: #ParseNodesDeclaringTemporaryVariableVisitor category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #ParseNodesDeclaringTemporaryVariableVisitor
	instanceVariableNames: 'temporaryVariable'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #TemporaryVariableDeclarationCounter category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #TemporaryVariableDeclarationCounter
	instanceVariableNames: 'temporaryVariable count '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'EB 5/28/2019 21:40:33'!
isNodeDeclaringTemporary: aParseNode

	^aParseNode temporaries anySatisfy: [ :temporaryVariableNode |
		temporaryVariableNode key = temporaryVariable
	].! !

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'EB 5/28/2019 20:58:20'!
visitBlockNode: aBlockNode

	(self isNodeDeclaringTemporary: aBlockNode) ifTrue: [ 
		self visitBlockNodeDeclaringTemporary: aBlockNode
	].
	super visitBlockNode: aBlockNode.! !

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'EB 5/28/2019 21:40:38'!
visitBlockNodeDeclaringTemporary: aBlockNode

	self subclassResponsibility.! !

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'EB 5/28/2019 20:58:33'!
visitMethodNode: aMethodNode

	(self isNodeDeclaringTemporary: aMethodNode) ifTrue: [
		self visitMethodNodeDeclaringTemporary: aMethodNode.
	].
	super visitMethodNode: aMethodNode.! !

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'visiting' stamp: 'EB 5/28/2019 21:40:42'!
visitMethodNodeDeclaringTemporary: aMethodNode

	self subclassResponsibility.! !

!ParseNodesDeclaringTemporaryVariableVisitor methodsFor: 'initialization' stamp: 'EB 5/28/2019 20:53:53'!
initializeFor: aTemporaryVariable

	temporaryVariable _ aTemporaryVariable.
! !


!ParseNodesDeclaringTemporaryVariableVisitor class methodsFor: 'instance creation' stamp: 'EB 5/28/2019 21:40:48'!
for: aTemporaryVariable

	^self new initializeFor: aTemporaryVariable.! !

!classDefinition: #TemporaryVariableDeclarationCounter category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #TemporaryVariableDeclarationCounter
	instanceVariableNames: 'count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
