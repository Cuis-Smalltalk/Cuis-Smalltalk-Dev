'From Cuis 5.0 of 7 November 2016 [latest update: #3766] on 28 May 2019 at 12:20:29 am'!
!classDefinition: #TemporaryVariableDeclarationCounter category: #'Tools-Refactoring'!
ParseNodeVisitor subclass: #TemporaryVariableDeclarationCounter
	instanceVariableNames: 'temporaryVariable count'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!TemporaryVariableDeclarationCounter methodsFor: 'initialization' stamp: 'EB 5/27/2019 20:46:01'!
initializeFor: aTemporaryVariable

	temporaryVariable _ aTemporaryVariable.
	count _ 0.! !

!TemporaryVariableDeclarationCounter methodsFor: 'visiting' stamp: 'EB 5/27/2019 20:53:26'!
visitBlockNode: aBlockNode

	| hasTemporaryVariable |
	
	super visitBlockNode: aBlockNode.
	hasTemporaryVariable _ aBlockNode temporaries anySatisfy: [ :temporaryVariableNode |
		temporaryVariableNode key = temporaryVariable ].
	hasTemporaryVariable ifTrue: [ count _ count + 1 ]! !

!TemporaryVariableDeclarationCounter methodsFor: 'count' stamp: 'EB 5/27/2019 20:46:23'!
count

	^count.! !


!TemporaryVariableDeclarationCounter class methodsFor: 'instance creation' stamp: 'EB 5/27/2019 20:32:01'!
for: aTemporaryVariable
	^self new initializeFor: aTemporaryVariable.! !


!TemporaryToInstanceVariable class methodsFor: 'error descriptions' stamp: 'EB 5/27/2019 20:20:10'!
temporaryExistsInOtherBlockErrorDescription

	^'The temporary exists in other blocks in this method; remove those first.'.! !

!TemporaryToInstanceVariable class methodsFor: 'assertions' stamp: 'EB 5/27/2019 20:45:47'!
assertNoOtherBlockIn: aMethodNode definesTemporaryNamed: aTemporaryVariableName

	| counter |
	
	counter _ TemporaryVariableDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: counter.
	counter count > 1 ifTrue: [
		self refactoringError: self temporaryExistsInOtherBlockErrorDescription ].! !


!TemporaryToInstanceVariable class methodsFor: 'instance creation' stamp: 'EB 5/27/2019 20:22:44'!
named: aTemporaryVariableName fromMethod: aMethodNode

	| methodClass |

	methodClass _ aMethodNode methodClass.
	
	self assertNoOtherBlockIn: aMethodNode definesTemporaryNamed: aTemporaryVariableName.
	self assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName.
	self assertNoOtherMethodIn: methodClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName. 
	self assertSubclassesOf: methodClass haventGotInstanceVariableNamed: aTemporaryVariableName.
	
	^self new initializeNamed: aTemporaryVariableName fromMethod: aMethodNode.! !

