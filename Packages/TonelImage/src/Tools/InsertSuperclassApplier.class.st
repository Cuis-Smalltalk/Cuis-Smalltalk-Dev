Class {
	#name : 'InsertSuperclassApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'browser',
		'newSuperclassName',
		'classToRefactor'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
InsertSuperclassApplier class >> on: aBrowser for: aClass [

	^self new initializeOn: aBrowser for: aClass
]

{ #category : 'refactoring - parameters request' }
InsertSuperclassApplier >> askNewSuperclassName [
	newSuperclassName := self
		request: 'Enter new superclass name:'.
	newSuperclassName := newSuperclassName withBlanksTrimmed asSymbol.
]

{ #category : 'refactoring - creation' }
InsertSuperclassApplier >> createRefactoring [

	^InsertSuperclass to: classToRefactor named: newSuperclassName
]

{ #category : 'initialization' }
InsertSuperclassApplier >> initializeOn: aBrowser for: aClass [
	browser := aBrowser.
	classToRefactor := aClass.
]

{ #category : 'refactoring - parameters request' }
InsertSuperclassApplier >> requestRefactoringParameters [

	self askNewSuperclassName
]

{ #category : 'refactoring - changes' }
InsertSuperclassApplier >> showChanges [
	
	browser changed: #classList.

]
