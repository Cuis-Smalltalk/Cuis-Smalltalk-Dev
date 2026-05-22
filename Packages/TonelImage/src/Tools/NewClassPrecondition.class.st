Class {
	#name : 'NewClassPrecondition',
	#superclass : 'RefactoringPrecondition',
	#instVars : [
		'newClassName',
		'system',
		'undeclared'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'error messages' }
NewClassPrecondition class >> errorMessageForAlreadyExistClassNamed: aNewClassName [

	^'Class named ', aNewClassName, ' already exist'
]

{ #category : 'error messages' }
NewClassPrecondition class >> errorMessageForAlreadyExistGlobalNamed: aNewClassName [

	^'There is already a global variable named ', aNewClassName 
]

{ #category : 'error messages' }
NewClassPrecondition class >> errorMessageForNewClassIsUndeclared: aNewClassName [

	^aNewClassName, ' is undeclared'
]

{ #category : 'instance creation' }
NewClassPrecondition class >> for: aNewClassName in: aSystem undeclared: anUndeclaredDictionary  [

	^self new initializeFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

]

{ #category : 'error messages' }
NewClassPrecondition class >> newClassNameCanNotBeEmptyErrorMessage [
	
	^'New class name can not be empty'
]

{ #category : 'error messages' }
NewClassPrecondition class >> newClassNameCanNotHaveSeparatorsErrorMessage [
	
	^'New class name can not have separators'
]

{ #category : 'error messages' }
NewClassPrecondition class >> newNameMustBeSymbolErrorMessage [
	
	^'New class name must be a symbol'
]

{ #category : 'error messages' }
NewClassPrecondition class >> newNameMustStartWithRightLetterErrorMessage [

	^'New class name must start with an uppercase letter'
]

{ #category : 'evaluation' }
NewClassPrecondition class >> valueFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary  [

	^(self for: aNewClassName in: aSystem undeclared: anUndeclaredDictionary) value
]

{ #category : 'evaluating - private' }
NewClassPrecondition >> assertNewClassNameDoesNotExistInSystem [

	system at: newClassName ifPresent: [ :value | 
		value isBehavior 
			ifTrue: [ self signalClassAlreadyExists]
			ifFalse: [  self signalGlobalAlreadyExists]].
]

{ #category : 'evaluating - private' }
NewClassPrecondition >> assertNewClassNameHasNoSeparators [

	(newClassName anySatisfy: [:aChar | aChar isSeparator]) 
		ifTrue: [ self signalNewClassNameCanNotHaveSeparators]
]

{ #category : 'evaluating - private' }
NewClassPrecondition >> assertNewClassNameIsNotDeclaredInUndeclared [

	(undeclared includesKey: newClassName) ifTrue: [ self signalNewClassIsUndeclared]
]

{ #category : 'evaluating - private' }
NewClassPrecondition >> assertNewClassNameIsNotEmpty [

	newClassName withBlanksTrimmed isEmpty ifTrue: [ self signalNewClassNameCanNotBeEmpty]
]

{ #category : 'evaluating - private' }
NewClassPrecondition >> assertNewClassNameStartsWithRightLetter [

	newClassName first isUppercase ifFalse: [ self signalNewNameMustStartWithRightLetter]
]

{ #category : 'evaluating - private' }
NewClassPrecondition >> assertNewClassNameSymbol [

	newClassName isSymbol ifFalse: [ self signalNewNameMustBeSymbol]
]

{ #category : 'initialization' }
NewClassPrecondition >> initializeFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary  [

	newClassName := aNewClassName.
	system := aSystem.
	undeclared := anUndeclaredDictionary 
]

{ #category : 'exceptions' }
NewClassPrecondition >> signalClassAlreadyExists [

	self refactoringError: (self class errorMessageForAlreadyExistClassNamed: newClassName).
]

{ #category : 'exceptions' }
NewClassPrecondition >> signalGlobalAlreadyExists [

	self refactoringError: (self class errorMessageForAlreadyExistGlobalNamed: newClassName)
]

{ #category : 'exceptions' }
NewClassPrecondition >> signalNewClassIsUndeclared [

	self refactoringError: (self class errorMessageForNewClassIsUndeclared: newClassName).
]

{ #category : 'exceptions' }
NewClassPrecondition >> signalNewClassNameCanNotBeEmpty [
	
	self refactoringError: self class newClassNameCanNotBeEmptyErrorMessage
]

{ #category : 'exceptions' }
NewClassPrecondition >> signalNewClassNameCanNotHaveSeparators [
	
	self refactoringError: self class newClassNameCanNotHaveSeparatorsErrorMessage 
]

{ #category : 'exceptions' }
NewClassPrecondition >> signalNewNameMustBeSymbol [

	self refactoringError: self class newNameMustBeSymbolErrorMessage.
]

{ #category : 'exceptions' }
NewClassPrecondition >> signalNewNameMustStartWithRightLetter [

	self refactoringError: self class newNameMustStartWithRightLetterErrorMessage.
]

{ #category : 'evaluating' }
NewClassPrecondition >> value [

	self assertNewClassNameIsNotEmpty.
	self assertNewClassNameSymbol.
	self assertNewClassNameStartsWithRightLetter.
	self assertNewClassNameHasNoSeparators.
	self assertNewClassNameDoesNotExistInSystem.
	self assertNewClassNameIsNotDeclaredInUndeclared.


]
