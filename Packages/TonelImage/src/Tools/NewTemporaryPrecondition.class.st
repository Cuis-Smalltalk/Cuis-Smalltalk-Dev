"
I am responsible for checking if a new temporary variable can be introduced in a specific block node of a method. If that is not possible, I raise a refactoring error.
"
Class {
	#name : 'NewTemporaryPrecondition',
	#superclass : 'RefactoringPrecondition',
	#instVars : [
		'newTemporaryVariableName',
		'methodNode',
		'blockNode'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'error messages' }
NewTemporaryPrecondition class >> errorMessageFor: aNewVariable canNotBeNamedDueToInstanceVariableDefinedIn: aClass [

	^ aNewVariable, ' cannot be used as a temporary variable name because it is defined as an instance variable in ', aClass name
]

{ #category : 'error messages' }
NewTemporaryPrecondition class >> errorMessageForEmptyTemporaryVariable [
	
	^ 'New variable can not be empty'
]

{ #category : 'error messages' }
NewTemporaryPrecondition class >> errorMessageForInvalidTemporaryVariable: aName [

	^ '''' , aName , ''' is not a valid temporary variable name'
]

{ #category : 'error messages' }
NewTemporaryPrecondition class >> errorMessageForNewTemporaryVariable: aNewVariable isAlreadyDefinedIn: aMethodNode [

	^ aNewVariable , ' is already defined in ' , aMethodNode classAndSelector
]

{ #category : 'error messages' }
NewTemporaryPrecondition class >> errorMessageForNewTemporaryVariableCanNotBeAReservedName: aName [

	^ '''', aName, ''' can not be used as temporary variable name because it is a reserved name'
]

{ #category : 'instance creation' }
NewTemporaryPrecondition class >> for: aNewTemporaryVariableName in: aBlockNode of: aMethodNode [

	^ self new initializeFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode
]

{ #category : 'evaluating' }
NewTemporaryPrecondition class >> valueFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode [

	^ (self for: aNewTemporaryVariableName in: aBlockNode of: aMethodNode) value
]

{ #category : 'pre-conditions' }
NewTemporaryPrecondition >> assertIsNotAReservedName [
	
	(ClassBuilder reservedNames includes: newTemporaryVariableName)
		ifTrue: [ self signalNewTemporaryVariableCanNotBeAReservedName ]
]

{ #category : 'pre-conditions' }
NewTemporaryPrecondition >> assertIsNotDeclaredInParentsOrChildrenScopes [

	(self isDeclaredInChildrenOfBlockNode or: [ self isDeclaredInParentsOfBlockNode ])
		ifTrue: [ self signalNewTemporaryVariableisAlreadyDefined ]
]

{ #category : 'pre-conditions' }
NewTemporaryPrecondition >> assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass [

	| classDefiningNewVariable |
	
	classDefiningNewVariable := methodNode methodClass
		whichClassDefinesInstanceVariable: newTemporaryVariableName ifNone: [ ^ self ].
		
	self signalNewVariableCanNotHideInstanceVariableDefinedIn: classDefiningNewVariable
]

{ #category : 'pre-conditions' }
NewTemporaryPrecondition >> assertIsNotEmpty [

	newTemporaryVariableName isEmpty ifTrue: [ self signalNewVariableCanNotBeEmpty ]
]

{ #category : 'pre-conditions' }
NewTemporaryPrecondition >> assertIsValidVariableName [

	| scannedNames |
	scannedNames := Scanner new scanFieldNames: newTemporaryVariableName.
	scannedNames size = 1 ifFalse: [ self signalInvalidTemporaryVariable ].
	scannedNames first = newTemporaryVariableName ifFalse: [ self signalInvalidTemporaryVariable ].
]

{ #category : 'initialization' }
NewTemporaryPrecondition >> initializeFor: aNewTemporaryVariableName in: aBlockNode of: aMethodNode [

	newTemporaryVariableName := aNewTemporaryVariableName.
	blockNode := aBlockNode.
	methodNode := aMethodNode
]

{ #category : 'private' }
NewTemporaryPrecondition >> isDeclaredInAnyOf: someBlockOrMethodNodes [

	^ someBlockOrMethodNodes anySatisfy: [ :node | node  hasLocallyArgumentOrTemporaryNamed: newTemporaryVariableName ]
]

{ #category : 'private' }
NewTemporaryPrecondition >> isDeclaredInChildrenOfBlockNode [

	blockNode nodesDo: [ :node |
		(node isBlockNode and: [ node hasLocallyArgumentOrTemporaryNamed: newTemporaryVariableName ]) ifTrue: [ ^ true ] ].

	^ false
]

{ #category : 'private' }
NewTemporaryPrecondition >> isDeclaredInParentsOfBlockNode [

	| parents |
	parents := (BlockNodeParentsFinder for: blockNode) parentsIn: methodNode.
	parents add: methodNode.
	^ self isDeclaredInAnyOf: parents
]

{ #category : 'exceptions' }
NewTemporaryPrecondition >> signalInvalidTemporaryVariable [

	self refactoringError: (self class errorMessageForInvalidTemporaryVariable: newTemporaryVariableName)
]

{ #category : 'exceptions' }
NewTemporaryPrecondition >> signalNewTemporaryVariableCanNotBeAReservedName [

	self refactoringError: (
		self class errorMessageForNewTemporaryVariableCanNotBeAReservedName: newTemporaryVariableName)
]

{ #category : 'exceptions' }
NewTemporaryPrecondition >> signalNewTemporaryVariableisAlreadyDefined [

	self refactoringError: (
		self class
			errorMessageForNewTemporaryVariable: newTemporaryVariableName
			isAlreadyDefinedIn: methodNode)
]

{ #category : 'exceptions' }
NewTemporaryPrecondition >> signalNewVariableCanNotBeEmpty [
	
	self refactoringError: self class errorMessageForEmptyTemporaryVariable
]

{ #category : 'exceptions' }
NewTemporaryPrecondition >> signalNewVariableCanNotHideInstanceVariableDefinedIn: aClass [

	self refactoringError: (
		self class
			errorMessageFor: newTemporaryVariableName
			canNotBeNamedDueToInstanceVariableDefinedIn: aClass)
]

{ #category : 'evaluating' }
NewTemporaryPrecondition >> value [

	self
		assertIsNotEmpty;
		assertIsValidVariableName;
		assertIsNotAReservedName;
		assertIsNotDefinedAsInstanceVariableInHierarchyOfMethodClass;
		assertIsNotDeclaredInParentsOrChildrenScopes
]
