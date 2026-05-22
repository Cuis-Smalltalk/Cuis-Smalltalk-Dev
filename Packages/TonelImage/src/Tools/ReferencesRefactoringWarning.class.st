Class {
	#name : 'ReferencesRefactoringWarning',
	#superclass : 'RefactoringWarning',
	#instVars : [
		'references',
		'primaryReferencee',
		'allreferenced'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'as yet unclassified' }
ReferencesRefactoringWarning class >> signal: aMessageText references: references of: primaryReferencee toAll: allReferenced  [

	(self with: aMessageText references: references of: primaryReferencee toAll: allReferenced) signal
]

{ #category : 'instance creation' }
ReferencesRefactoringWarning class >> with: aMessageText references: references of: primaryReferencee toAll: allReferenced [

	^self new initializeWith: aMessageText references: references of: primaryReferencee toAll: allReferenced
]

{ #category : 'references' }
ReferencesRefactoringWarning >> anyReference [
	
	^references anyOne 
]

{ #category : 'initialization' }
ReferencesRefactoringWarning >> initializeWith: aMessageText references: aReferences of: aPrimaryReferencee toAll: anAllReferenced [

	self messageText: aMessageText.
	references := aReferences.
	primaryReferencee := aPrimaryReferencee.
	allreferenced := anAllReferenced 
]

{ #category : 'references' }
ReferencesRefactoringWarning >> numberOfReferences [
	
	^references size
]

{ #category : 'accessing' }
ReferencesRefactoringWarning >> referencee [
	
	^primaryReferencee 
]

{ #category : 'accessing' }
ReferencesRefactoringWarning >> references [
	
	^references
]
