Class {
	#name : 'TransferInstanceVariableApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'browser',
		'classToRefactor',
		'instanceVariableName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
TransferInstanceVariableApplier class >> on: aBrowser at: aClassToRefactor [
	
	^self on: aBrowser for: nil at: aClassToRefactor
]

{ #category : 'instance creation' }
TransferInstanceVariableApplier class >> on: aBrowser for: anInstanceVariableName at:  aClassToRefactor  [
	
	^self new initializeOn: aBrowser for: anInstanceVariableName at: aClassToRefactor
]

{ #category : 'refactoring - parameters request' }
TransferInstanceVariableApplier >> chooseInstanceVariable [

	instanceVariableName ifNotNil: [ ^self ].

	classToRefactor instVarNames size = 1 ifTrue: [^instanceVariableName := classToRefactor instVarNames first].

	classToRefactor
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :anInstanceVariable | ^instanceVariableName := anInstanceVariable ].

	self endRequest 
]

{ #category : 'refactoring - changes' }
TransferInstanceVariableApplier >> informChangesToBrowser [

	browser acceptedContentsChanged.
]

{ #category : 'initialization' }
TransferInstanceVariableApplier >> initializeOn: aBrowserWindow for: anInstanceVariableName at: aClassToRefactor  [
	
	browser := aBrowserWindow.
	classToRefactor := aClassToRefactor.
	instanceVariableName := anInstanceVariableName
]

{ #category : 'refactoring - parameters request' }
TransferInstanceVariableApplier >> requestRefactoringParameters [

	self chooseInstanceVariable
]

{ #category : 'refactoring - parameters request' }
TransferInstanceVariableApplier >> selectVariableLabel [
	
	self subclassResponsibility 
]

{ #category : 'refactoring - changes' }
TransferInstanceVariableApplier >> showChanges [

	self informChangesToBrowser
]
