Class {
	#name : 'RefactoringTest',
	#superclass : 'DynamicallyCodeCreationTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
RefactoringTest >> assert: expected equals: actual [
	"This tests assumes the order of arguments used by Cuis before July 31, 2023,
	and also used by Squeak."

	super assert: actual equals: expected
]

{ #category : 'assertions' }
RefactoringTest >> assertCreation: aCreationBlock failsWith: aStringOrBlock [

	self 
		should: aCreationBlock 
		raise: self refactoringError
		withMessageText: aStringOrBlock 
]

{ #category : 'assertions' }
RefactoringTest >> assertCreation: aCreationBlock warnsWith: aStringOrBlock [

	| warningSignaled |
	
	warningSignaled := false.
	aCreationBlock 
		on: self refactoringWarning
		do: [ :aWarning |
			self assert: aStringOrBlock value equals: aWarning messageText.
			warningSignaled := true.
			aWarning resume ].

	self assert: warningSignaled description: 'Warning was not signaled'
]

{ #category : 'exceptions' }
RefactoringTest >> canNotRefactorDueToReferencesRefactoringError [
	
	^ Refactoring canNotRefactorDueToReferencesErrorClass - MessageNotUnderstood.
]

{ #category : 'exceptions' }
RefactoringTest >> refactoringError [

	^ Refactoring refactoringErrorClass - MessageNotUnderstood.
]

{ #category : 'exceptions' }
RefactoringTest >> refactoringWarning [

	^ Refactoring refactoringWarningClass - MessageNotUnderstood.
]

{ #category : 'exceptions' }
RefactoringTest >> referencesRefactoringWarning [

	^ Refactoring referencesWarningClass - MessageNotUnderstood.
]
