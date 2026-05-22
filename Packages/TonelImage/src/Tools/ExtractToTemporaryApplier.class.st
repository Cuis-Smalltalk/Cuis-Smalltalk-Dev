Class {
	#name : 'ExtractToTemporaryApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'intervalToExtract',
		'methodToExtractCodeFrom',
		'newVariable',
		'codeProvider'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
ExtractToTemporaryApplier class >> assertCanApplyRefactoringOn: aMethodToRefactor at: anIntervalToExtract [

	ExtractToTemporary
		assertCanApplyRefactoringOn: aMethodToRefactor
		at: anIntervalToExtract
]

{ #category : 'instance creation' }
ExtractToTemporaryApplier class >> on: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom [

	self assertCanApplyRefactoringOn: aMethodToExtractCodeFrom at: anIntervalToExtract.

	^ self new initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom
]

{ #category : 'refactoring - parameters request' }
ExtractToTemporaryApplier >> askNewVariableName [

	newVariable := (self request: 'Enter new temp name:' initialAnswer: '') withBlanksTrimmed
]

{ #category : 'refactoring - creation' }
ExtractToTemporaryApplier >> createRefactoring [

	^ self refactoringClass
		named: newVariable
		at: intervalToExtract
		from: methodToExtractCodeFrom
]

{ #category : 'initialization' }
ExtractToTemporaryApplier >> initializeOn: aCodeProvider for: anIntervalToExtract of: aMethodToExtractCodeFrom [

	codeProvider := aCodeProvider.
	intervalToExtract := anIntervalToExtract.
	methodToExtractCodeFrom := aMethodToExtractCodeFrom.
]

{ #category : 'refactoring - creation' }
ExtractToTemporaryApplier >> refactoringClass [

	^ ExtractToTemporary
]

{ #category : 'refactoring - parameters request' }
ExtractToTemporaryApplier >> requestRefactoringParameters [

	self askNewVariableName
]

{ #category : 'refactoring - changes' }
ExtractToTemporaryApplier >> showChanges [

	codeProvider currentMethodRefactored
]
