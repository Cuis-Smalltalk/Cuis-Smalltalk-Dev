Class {
	#name : 'AddInstanceVariableApplier',
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
AddInstanceVariableApplier class >> on: aBrowser for: aClassToRefactor [
	
	^self new initializeOn: aBrowser for: aClassToRefactor 

]

{ #category : 'refactoring - parameters request' }
AddInstanceVariableApplier >> askNewVariableName [
		
	newInstanceVariable := self request: self newVariableNameLabel. 
	newInstanceVariable := newInstanceVariable withBlanksTrimmed 
]

{ #category : 'refactoring - creation' }
AddInstanceVariableApplier >> createRefactoring [
		
	^AddInstanceVariable named: newInstanceVariable to: classToRefactor.
	
]

{ #category : 'refactoring - changes' }
AddInstanceVariableApplier >> informChangesToBrowser [
		
	browser acceptedContentsChanged
]

{ #category : 'initialization' }
AddInstanceVariableApplier >> initializeOn: aBrowser for: aClassToRefactor  [
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor 
]

{ #category : 'refactoring - parameters request' }
AddInstanceVariableApplier >> newVariableNameLabel [
	
	^'Enter new variable name:'
]

{ #category : 'refactoring - parameters request' }
AddInstanceVariableApplier >> requestRefactoringParameters [

	self askNewVariableName
]

{ #category : 'refactoring - changes' }
AddInstanceVariableApplier >> showChanges [

	self informChangesToBrowser
]
