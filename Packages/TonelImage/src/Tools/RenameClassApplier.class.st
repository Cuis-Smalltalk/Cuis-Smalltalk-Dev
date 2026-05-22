Class {
	#name : 'RenameClassApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'classToRename',
		'newClassName'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
RenameClassApplier class >> for: aClass [

	^self new initializeFor: aClass
]

{ #category : 'refactoring - parameters request' }
RenameClassApplier >> askNewClassName [

	newClassName := self request: 'Enter new name:' initialAnswer: classToRename name.
	newClassName := newClassName withBlanksTrimmed asSymbol.
	
]

{ #category : 'refactoring - creation' }
RenameClassApplier >> createRefactoring [

	^RenameClass from: classToRename to: newClassName in: Smalltalk undeclared: Undeclared.
	

]

{ #category : 'initialization' }
RenameClassApplier >> initializeFor: aClass [

	classToRename := aClass.
	
]

{ #category : 'refactoring - changes' }
RenameClassApplier >> openChangedMethods [

	changes ifNotEmpty: [ 
		MethodSetWindow openMessageList: changes label: 'Renamed references'  autoHighlight: newClassName allOccurrences: false ]

]

{ #category : 'refactoring - parameters request' }
RenameClassApplier >> requestRefactoringParameters [

	self askNewClassName
]

{ #category : 'refactoring - changes' }
RenameClassApplier >> showChanges [

	self openChangedMethods

]
