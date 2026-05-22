Class {
	#name : 'ChangeKeywordsSelectorOrderApplier',
	#superclass : 'ChangeSelectorKeepingParametersApplier',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
ChangeKeywordsSelectorOrderApplier class >> assertCanApplyRefactoringFor: aSelector in: aSelectedClass [

	ChangeKeywordsSelectorOrder assertIsKeywordWithMoreThanOneParameter: aSelector 
]

{ #category : 'refactoring - creation' }
ChangeKeywordsSelectorOrderApplier >> refactoringClass [

	^ChangeKeywordsSelectorOrder 
]
