Class {
	#name : 'RemoveAllUnreferencedInstanceVariablesApplier',
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
RemoveAllUnreferencedInstanceVariablesApplier class >> on: aBrowser for: aClassToRefactor [
	
	^self new initializeOn: aBrowser for: aClassToRefactor 

]

{ #category : 'refactoring - creation' }
RemoveAllUnreferencedInstanceVariablesApplier >> createRefactoring [
		
	^RemoveAllUnreferencedInstanceVariables from: classToRefactor 
]

{ #category : 'refactoring - changes' }
RemoveAllUnreferencedInstanceVariablesApplier >> informChangesToBrowser [
		
	browser acceptedContentsChanged
]

{ #category : 'initialization' }
RemoveAllUnreferencedInstanceVariablesApplier >> initializeOn: aBrowser for: aClassToRefactor  [
	
	browser := aBrowser.
	classToRefactor := aClassToRefactor 
]

{ #category : 'refactoring - parameters request' }
RemoveAllUnreferencedInstanceVariablesApplier >> requestRefactoringParameters [

	
]

{ #category : 'refactoring - changes' }
RemoveAllUnreferencedInstanceVariablesApplier >> showChanges [

	| removedInstanceVariablesMessage |
	
	self informChangesToBrowser.
	removedInstanceVariablesMessage := changes isEmpty 
		ifTrue: [ 'No instance variable was removed' ]
		ifFalse: [ changes size = 1
			ifTrue: [ changes first, ' was removed' ]
			ifFalse: [ changes asCommaStringAnd, ' were removed' ]].
			
	self inform: removedInstanceVariablesMessage
]
