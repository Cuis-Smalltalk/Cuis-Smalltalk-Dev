Class {
	#name : 'RenameInstanceVariableApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'classToRefactor',
		'oldInstanceVariable',
		'newInstanceVariable',
		'browser'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
RenameInstanceVariableApplier class >> on: aBrowser at: aClassToRefactor [
	
	^self new initializeOn: aBrowser for: nil at: aClassToRefactor 

]

{ #category : 'instance creation' }
RenameInstanceVariableApplier class >> on: aBrowser for: anInstanceVariableName at:  aClassToRefactor  [
	
	^self new initializeOn: aBrowser for: anInstanceVariableName at: aClassToRefactor 
]

{ #category : 'refactoring - parameters request' }
RenameInstanceVariableApplier >> askNewVariableName [
		
	newInstanceVariable := self request: 'Enter new name:' initialAnswer: oldInstanceVariable. 
	newInstanceVariable := newInstanceVariable withBlanksTrimmed 
]

{ #category : 'refactoring - parameters request' }
RenameInstanceVariableApplier >> chooseInstanceVariable [
	
	oldInstanceVariable ifNotNil: [ ^self ].
		
	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :anOldInstanceVariable | ^oldInstanceVariable := anOldInstanceVariable ].
	self endRequest 
]

{ #category : 'refactoring - creation' }
RenameInstanceVariableApplier >> createRefactoring [
		
	^RenameInstanceVariable from: oldInstanceVariable to: newInstanceVariable in: classToRefactor.
	
]

{ #category : 'refactoring - changes' }
RenameInstanceVariableApplier >> informChangesToBrowser [
	
	browser currentMethodRefactored
]

{ #category : 'initialization' }
RenameInstanceVariableApplier >> initializeOn: aBrowserWindow for: anOldInstanceVariable at: aClassToRefactor  [
	
	browser := aBrowserWindow.
	classToRefactor := aClassToRefactor.
	oldInstanceVariable := anOldInstanceVariable 
]

{ #category : 'refactoring - parameters request' }
RenameInstanceVariableApplier >> requestRefactoringParameters [

	self 
		chooseInstanceVariable;
		askNewVariableName
]

{ #category : 'refactoring - parameters request' }
RenameInstanceVariableApplier >> selectVariableLabel [
	
	^'Select instance variable to rename'
]

{ #category : 'refactoring - changes' }
RenameInstanceVariableApplier >> showChanges [

	self informChangesToBrowser
]
