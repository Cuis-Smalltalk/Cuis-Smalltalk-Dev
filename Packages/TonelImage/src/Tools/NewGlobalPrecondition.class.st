Class {
	#name : 'NewGlobalPrecondition',
	#superclass : 'RefactoringPrecondition',
	#instVars : [
		'system',
		'newName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'error messages' }
NewGlobalPrecondition class >> errorMessageForAlreadyExistClassNamed: aNewName [

	^'Class named ', aNewName, ' already exist'
]

{ #category : 'error messages' }
NewGlobalPrecondition class >> errorMessageForAlreadyExistGlobalNamed: aNewName [

	^'There is already a global variable named ', aNewName 
]

{ #category : 'instance creation' }
NewGlobalPrecondition class >> for: aNewName in: aSystem   [

	^self new initializeFor: aNewName in: aSystem 
]

{ #category : 'error messages' }
NewGlobalPrecondition class >> newNameCanNotBeEmptyErrorMessage [
	
	^'New name can not be empty'
]

{ #category : 'error messages' }
NewGlobalPrecondition class >> newNameCanNotHaveSeparatorsErrorMessage [
	
	^'New name can not have separators'
]

{ #category : 'error messages' }
NewGlobalPrecondition class >> newNameMustBeSymbolErrorMessage [
	
	^'New name must be a symbol'
]

{ #category : 'evaluation' }
NewGlobalPrecondition class >> valueFor: aNewName in: aSystem   [

	^(self for: aNewName in: aSystem) value
]

{ #category : 'evaluating - private' }
NewGlobalPrecondition >> assertNewNameDoesNotExistInSystem [

	system at: newName ifPresent: [ :value | 
		value isBehavior 
			ifTrue: [ self signalClassAlreadyExists ]
			ifFalse: [ self signalGlobalAlreadyExists ]].
]

{ #category : 'evaluating - private' }
NewGlobalPrecondition >> assertNewNameHasNoSeparators [

	(newName anySatisfy: [:aChar | aChar isSeparator]) 
		ifTrue: [ self signalNewNameCanNotHaveSeparators ]
]

{ #category : 'evaluating - private' }
NewGlobalPrecondition >> assertNewNameIsNotEmpty [

	newName withBlanksTrimmed isEmpty ifTrue: [ self signalNewNameCanNotBeEmpty]
]

{ #category : 'evaluating - private' }
NewGlobalPrecondition >> assertNewNameIsSymbol [

	newName isSymbol ifFalse: [ self signalNewNameMustBeSymbol]
]

{ #category : 'initialization' }
NewGlobalPrecondition >> initializeFor: aNewName in: aSystem   [

	newName := aNewName.
	system := aSystem.

]

{ #category : 'exceptions' }
NewGlobalPrecondition >> signalClassAlreadyExists [

	self refactoringError: (self class errorMessageForAlreadyExistClassNamed: newName).
]

{ #category : 'exceptions' }
NewGlobalPrecondition >> signalGlobalAlreadyExists [

	self refactoringError: (self class errorMessageForAlreadyExistGlobalNamed: newName)
]

{ #category : 'exceptions' }
NewGlobalPrecondition >> signalNewNameCanNotBeEmpty [
	
	self refactoringError: self class newNameCanNotBeEmptyErrorMessage
]

{ #category : 'exceptions' }
NewGlobalPrecondition >> signalNewNameCanNotHaveSeparators [
	
	self refactoringError: self class newNameCanNotHaveSeparatorsErrorMessage 
]

{ #category : 'exceptions' }
NewGlobalPrecondition >> signalNewNameMustBeSymbol [

	self refactoringError: self class newNameMustBeSymbolErrorMessage.
]

{ #category : 'evaluating' }
NewGlobalPrecondition >> value [

	self 
		assertNewNameIsNotEmpty;
		assertNewNameIsSymbol;
		assertNewNameHasNoSeparators;
		assertNewNameDoesNotExistInSystem.


]
