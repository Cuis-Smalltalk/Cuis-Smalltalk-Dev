"
I am responsible for checking if a new method with a given name can be introduced in a specific class. If that is not possible, I raise a refactoring error.
"
Class {
	#name : 'NewSelectorPrecondition',
	#superclass : 'RefactoringPrecondition',
	#instVars : [
		'selectorToValidate',
		'classToDefineSelector'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
NewSelectorPrecondition class >> for: aSelectorToValidate on: aClass  [

	^ self new initializeFor: aSelectorToValidate on: aClass
]

{ #category : 'error messages' }
NewSelectorPrecondition class >> invalidCharacterInsideNewSelectorErrorMessage [

	^ 'New selector should only contain letters, numbers or _'
]

{ #category : 'error messages' }
NewSelectorPrecondition class >> invalidStartingCharacterOfNewSelectorErrorMessage [

	^ 'New selector should begin with a lowercase letter or _'
]

{ #category : 'error messages' }
NewSelectorPrecondition class >> newSelectorCanNotBeEmptyErrorMessage [

	^ 'New selector can not be empty'
]

{ #category : 'error messages' }
NewSelectorPrecondition class >> newSelectorCanNotContainSeparatorsErrorMessage [

	^ 'New selector can not contain separators'
]

{ #category : 'evaluating' }
NewSelectorPrecondition class >> valueFor: aSelectorToValidate on: aClass [

	^ (self for: aSelectorToValidate on: aClass) value
]

{ #category : 'warning messages' }
NewSelectorPrecondition class >> warningMessageFor: existingSelector isAlreadyDefinedIn: classDefiningSelector  [

	^ existingSelector , ' is already defined in ' , classDefiningSelector name
]

{ #category : 'evaluating - private' }
NewSelectorPrecondition >> assertNewSelectorBeginsWithAValidCharacter [

	selectorToValidate first isValidStartOfIdentifiers 
		ifFalse: [ self signalNewSelectorBeginsWithAnInvalidCharacter ]
]

{ #category : 'evaluating - private' }
NewSelectorPrecondition >> assertNewSelectorContainsOnlyValidCharacters [

	selectorToValidate isValidSelector
		ifFalse: [ self signalNewSelectorContainsInvalidCharactersError ]
]

{ #category : 'evaluating - private' }
NewSelectorPrecondition >> assertNewSelectorDoesNotContainSeparators [

	(selectorToValidate anySatisfy: [ :character | character isSeparator ])
		ifTrue: [ self signalNewSelectorCanNotContainSeparatorsError ]
]

{ #category : 'evaluating - private' }
NewSelectorPrecondition >> assertNewSelectorIsNotAlreadyDefinedWithinTheClassHierarchy [

	(classToDefineSelector whichClassIncludesSelector: selectorToValidate)
		ifNotNil: [ :classDefiningSelector | self warn: selectorToValidate isAlreadyDefinedIn: classDefiningSelector ]
]

{ #category : 'evaluating - private' }
NewSelectorPrecondition >> assertNewSelectorIsNotEmpty [

	selectorToValidate ifEmpty: [ self signalNewSelectorCanNotBeEmptyError ]
]

{ #category : 'initialization' }
NewSelectorPrecondition >> initializeFor: aSelectorToValidate on: aClassToDefineSelector [

	selectorToValidate := aSelectorToValidate.
	classToDefineSelector := aClassToDefineSelector
]

{ #category : 'exceptions' }
NewSelectorPrecondition >> signalNewSelectorBeginsWithAnInvalidCharacter [

	self refactoringError: self class invalidStartingCharacterOfNewSelectorErrorMessage
]

{ #category : 'exceptions' }
NewSelectorPrecondition >> signalNewSelectorCanNotBeEmptyError [

	self refactoringError: self class newSelectorCanNotBeEmptyErrorMessage
]

{ #category : 'exceptions' }
NewSelectorPrecondition >> signalNewSelectorCanNotContainSeparatorsError [

	self refactoringError: self class newSelectorCanNotContainSeparatorsErrorMessage
]

{ #category : 'exceptions' }
NewSelectorPrecondition >> signalNewSelectorContainsInvalidCharactersError [

	self refactoringError: self class invalidCharacterInsideNewSelectorErrorMessage
]

{ #category : 'evaluating' }
NewSelectorPrecondition >> value [

	self
		assertNewSelectorIsNotEmpty;
		assertNewSelectorDoesNotContainSeparators;
		assertNewSelectorBeginsWithAValidCharacter;
		assertNewSelectorContainsOnlyValidCharacters;
		assertNewSelectorIsNotAlreadyDefinedWithinTheClassHierarchy
]

{ #category : 'warnings' }
NewSelectorPrecondition >> warn: aSelector isAlreadyDefinedIn: aClassDefiningSelector [

	self refactoringWarning: (self class warningMessageFor: aSelector isAlreadyDefinedIn: aClassDefiningSelector)
]
