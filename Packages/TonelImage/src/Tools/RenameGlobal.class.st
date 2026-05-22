Class {
	#name : 'RenameGlobal',
	#superclass : 'RenameObject',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RenameGlobal class >> assert: anOldName isDefinedIn: aSystem [
	
	(aSystem bindingOf: anOldName) ifNil: [ self signalGlobalNotDefined: anOldName ]
]

{ #category : 'pre-conditions' }
RenameGlobal class >> assert: anOldName isNotEqualTo: aNewName [

	anOldName = aNewName ifTrue: [ self signalNewNameEqualsOldName]
]

{ #category : 'pre-conditions' }
RenameGlobal class >> assertIsNotClass: anOldName [

	(Smalltalk classNamed: anOldName) ifNotNil: [ self signalGlobalToRenameCanNotBeClass]
]

{ #category : 'pre-conditions' }
RenameGlobal class >> assertIsNotEmpty: anOldName [

	anOldName isEmpty ifTrue: [ self signalOldNameIsEmpty ]
]

{ #category : 'exceptions' }
RenameGlobal class >> errorMessageForGlobalNotDefined: anOldName [

	^ anOldName asPlainString, ' is not defined as global variable'
]

{ #category : 'instance creation' }
RenameGlobal class >> from: anOldName to: aNewName [

	^self from: anOldName to: aNewName in: Smalltalk
	
]

{ #category : 'instance creation' }
RenameGlobal class >> from: anOldName to: aNewName in: aSystem [

	self assertIsNotEmpty: anOldName.
	self assert: anOldName isDefinedIn: aSystem.
	self assertIsNotClass: anOldName.
	self assert: anOldName isNotEqualTo: aNewName.
	self newGlobalPreconditionClass valueFor: aNewName in: aSystem.
	
	^self new initializeFrom: anOldName to: aNewName in: aSystem 
]

{ #category : 'exceptions' }
RenameGlobal class >> globalCanNotBeClassErrorMessage [
	
	^'Global to rename can not be a class'
]

{ #category : 'pre-conditions' }
RenameGlobal class >> newGlobalPreconditionClass [

	^NewGlobalPrecondition 
]

{ #category : 'exceptions' }
RenameGlobal class >> newNameEqualsOldNameErrorMessage [
	
	^'New name is equal to the old one'
]

{ #category : 'exceptions' }
RenameGlobal class >> oldNameCanNotBeEmptyErrorMessage [
	
	^'Global variable name to rename can not be empty'
]

{ #category : 'exceptions' }
RenameGlobal class >> signalGlobalNotDefined: anOldName [

	self refactoringError: (self errorMessageForGlobalNotDefined: anOldName)
]

{ #category : 'exceptions' }
RenameGlobal class >> signalGlobalToRenameCanNotBeClass [
	
	self refactoringError: self globalCanNotBeClassErrorMessage
]

{ #category : 'exceptions' }
RenameGlobal class >> signalNewNameEqualsOldName [

	self refactoringError: self newNameEqualsOldNameErrorMessage.
]

{ #category : 'exceptions' }
RenameGlobal class >> signalOldNameIsEmpty [
	
	self refactoringError: self oldNameCanNotBeEmptyErrorMessage
]

{ #category : 'applying' }
RenameGlobal >> apply [
	
	| renamedReferences |
	
	system at: newName put: (system at: oldName).
	renamedReferences := self renameReferences.
	system removeKey: oldName.
	
	^renamedReferences 
	
]

{ #category : 'initialization' }
RenameGlobal >> initializeFrom: anOldName to: aNewName in: aSystem   [

	oldName := anOldName.
	newName := aNewName.
	system := aSystem.
	
	
]

{ #category : 'applying - private' }
RenameGlobal >> referencesToRename [

	^ self referencesToOldName
]

{ #category : 'applying - private' }
RenameGlobal >> rejectReferencesToClassVariablesFrom: references [
	
	^references reject: [ :aMethodReference | self referencesOldName: aMethodReference ].
]
