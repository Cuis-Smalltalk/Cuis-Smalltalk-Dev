Class {
	#name : 'RenameSelectorApplier',
	#superclass : 'ChangeSelectorKeepingParametersApplier',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RenameSelectorApplier class >> assertCanApplyRefactoringFor: aSelector in: aSelectedClass [

	
]

{ #category : 'refactoring - creation' }
RenameSelectorApplier >> createRefactoring [
	
	^self refactoringClass from: oldSelector to: newSelector implementors: implementors senders: senders.
	
]

{ #category : 'refactoring - creation' }
RenameSelectorApplier >> refactoringClass [

	^RenameSelector 
]
