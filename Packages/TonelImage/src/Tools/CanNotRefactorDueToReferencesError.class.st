Class {
	#name : 'CanNotRefactorDueToReferencesError',
	#superclass : 'RefactoringError',
	#instVars : [
		'references',
		'referencee'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'signaling' }
CanNotRefactorDueToReferencesError class >> signal: aMessageText references: references to: aReferencee [

	(self with: aMessageText references: references to: aReferencee) signal
]

{ #category : 'instance creation' }
CanNotRefactorDueToReferencesError class >> with: aMessageText references: references to: aReferencee [

	^self new initializeWith: aMessageText references: references to: aReferencee
]

{ #category : 'references' }
CanNotRefactorDueToReferencesError >> anyReference [
	
	^references anyOne 
]

{ #category : 'initialization' }
CanNotRefactorDueToReferencesError >> initializeWith: aMessageText references: aCollectionOfReferences to: aReferencee [

	self messageText: aMessageText.
	references := aCollectionOfReferences.
	referencee := aReferencee 
]

{ #category : 'references' }
CanNotRefactorDueToReferencesError >> numberOfReferences [
	
	^references size
]

{ #category : 'referencee' }
CanNotRefactorDueToReferencesError >> referencee [

	^referencee 
]

{ #category : 'references' }
CanNotRefactorDueToReferencesError >> references [

	^references copy
]
