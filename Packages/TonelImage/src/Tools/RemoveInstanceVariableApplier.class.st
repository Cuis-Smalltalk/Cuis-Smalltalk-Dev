Class {
	#name : 'RemoveInstanceVariableApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'classToRefactor',
		'browser',
		'variableToRemove'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
RemoveInstanceVariableApplier class >> on: aBrowser for: aClassToRefactor [
	
	^self new initializeOn: aBrowser for: aClassToRefactor 

]

{ #category : 'refactoring - parameters request' }
RemoveInstanceVariableApplier >> chooseInstanceVariable [

	classToRefactor 
		chooseDefiningInstanceVariableAlphabeticallyWith: self selectVariableLabel
		thenDo: [ :aVariableToRemove | ^variableToRemove := aVariableToRemove ].
	self endRequest 

	
]

{ #category : 'refactoring - creation' }
RemoveInstanceVariableApplier >> createRefactoring [
		
	^RemoveInstanceVariable named: variableToRemove from: classToRefactor 
]

{ #category : 'refactoring - changes' }
RemoveInstanceVariableApplier >> informChangesToBrowser [
		
	browser acceptedContentsChanged
]

{ #category : 'initialization' }
RemoveInstanceVariableApplier >> initializeOn: aBrowser for: aClassToRefactor  [
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor 
]

{ #category : 'refactoring - parameters request' }
RemoveInstanceVariableApplier >> requestRefactoringParameters [

	self chooseInstanceVariable.

	
]

{ #category : 'refactoring - parameters request' }
RemoveInstanceVariableApplier >> selectVariableLabel [
	
	^'Select instance variable to remove'
]

{ #category : 'refactoring - changes' }
RemoveInstanceVariableApplier >> showChanges [

	self informChangesToBrowser
]
