Class {
	#name : 'RenameClass',
	#superclass : 'RenameObject',
	#instVars : [
		'classToRename',
		'undeclared'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
RenameClass class >> assert: aClass isNotNamed: aNewName [

	aClass name = aNewName ifTrue: [ self signalNewNameEqualsOldName]
]

{ #category : 'pre-conditions' }
RenameClass class >> assertIsNotMeta: aBehavior [

	aBehavior isMeta ifTrue: [ self signalClassToRenameCanNotBeMetaclass]
]

{ #category : 'exceptions' }
RenameClass class >> classToRenameCanNotBeMetaclassErrorMessage [
	
	^'Class to rename can not be a metaclass'
]

{ #category : 'instance creation' }
RenameClass class >> from: aClass to: aNewClassName [

	^self from: aClass to: aNewClassName in: Smalltalk
	
]

{ #category : 'instance creation' }
RenameClass class >> from: aClass to: aNewClassName in: aSystem [

	^self from: aClass to: aNewClassName in: aSystem undeclared: Undeclared 
	
	
]

{ #category : 'instance creation' }
RenameClass class >> from: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary  [

	self assertIsNotMeta: aClass.
	self assert: aClass isNotNamed: aNewClassName.
	self newClassPreconditionClass valueFor: aNewClassName in: aSystem undeclared: anUndeclaredDictionary.
	
	^self new initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary 

]

{ #category : 'pre-conditions' }
RenameClass class >> newClassPreconditionClass [

	^NewClassPrecondition 
]

{ #category : 'exceptions' }
RenameClass class >> newNameEqualsOldNameErrorMessage [
	
	^'New class name is equal to the old one'
]

{ #category : 'exceptions' }
RenameClass class >> signalClassToRenameCanNotBeMetaclass [
	
	self refactoringError: self classToRenameCanNotBeMetaclassErrorMessage
]

{ #category : 'exceptions' }
RenameClass class >> signalNewNameEqualsOldName [

	self refactoringError: self newNameEqualsOldNameErrorMessage.
]

{ #category : 'applying' }
RenameClass >> apply [

	classToRename safeRenameTo: newName.
	^self renameReferences.

	
]

{ #category : 'initialization' }
RenameClass >> initializeFrom: aClass to: aNewClassName in: aSystem undeclared: anUndeclaredDictionary [

	classToRename := aClass.
	oldName := aClass name.
	newName := aNewClassName.
	system := aSystem.
	undeclared := anUndeclaredDictionary.

	
]

{ #category : 'applying - private' }
RenameClass >> referencesNewClassName: aMethodReference [

	^self references: aMethodReference classVarNamed: newName 
]

{ #category : 'accessing' }
RenameClass >> referencesToOldClass [

	^system allCallsOn: newName
]

{ #category : 'applying - private' }
RenameClass >> referencesToRename [

	^ (self referencesToOldClass asSet, self referencesToOldName asSet) asOrderedCollection
]

{ #category : 'applying - private' }
RenameClass >> rejectReferencesToClassVariablesFrom: references [

	^references reject: [ :aMethodReference | (self referencesOldName: aMethodReference) or: [ self referencesNewClassName: aMethodReference ] ].
]
