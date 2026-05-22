"
I represent a precondition, a prerequisite for a refactoring to be evaluated successfully. My public instance protocol includes only one message, #value, which could raise either a RefactoringError (in case the refactoring cannot be performed) or a RefactoringWarning (in case something needs the programmer's attention, but it can be resumed to continue with the refactoring).
"
Class {
	#name : 'RefactoringPrecondition',
	#superclass : 'Object',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'exceptions' }
RefactoringPrecondition >> refactoringError: aMessage [

	Refactoring refactoringError: aMessage.
]

{ #category : 'warnings' }
RefactoringPrecondition >> refactoringWarning: aMessageText [

	^ Refactoring refactoringWarning: aMessageText 
]

{ #category : 'evaluating' }
RefactoringPrecondition >> value [

	self subclassResponsibility
]
