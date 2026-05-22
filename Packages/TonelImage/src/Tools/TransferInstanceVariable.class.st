Class {
	#name : 'TransferInstanceVariable',
	#superclass : 'Refactoring',
	#instVars : [
		'classToRefactor',
		'instanceVariableToMove'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
TransferInstanceVariable class >> assert: aClassToRefactor hasInstanceVariable: anInstanceVariable [

	(aClassToRefactor definesInstanceVariableNamed: anInstanceVariable) ifFalse: [self refactoringError: self instanceVariableDoesNotExistOnClassToRefactor].
	
	
]

{ #category : 'pre-conditions' }
TransferInstanceVariable class >> assertCanMove: anInstanceVariable from: aClassToRefactor [

	self subclassResponsibility 
]

{ #category : 'error descriptions' }
TransferInstanceVariable class >> instanceVariableDoesNotExistOnClassToRefactor [
	
	^ 'Instance variable does not exist on class to refactor'
]

{ #category : 'instance creation' }
TransferInstanceVariable class >> named: anInstanceVariable from: aClassToRefactor [
	
	self assertCanMove: anInstanceVariable from: aClassToRefactor.
	
	^self new initializeNamed: anInstanceVariable from: aClassToRefactor
]

{ #category : 'initialization' }
TransferInstanceVariable >> initializeNamed: anInstanceVariableToMove from: aClassToRefactor [

	instanceVariableToMove := anInstanceVariableToMove.
	classToRefactor := aClassToRefactor.
]
