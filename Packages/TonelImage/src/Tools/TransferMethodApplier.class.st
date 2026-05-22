Class {
	#name : 'TransferMethodApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'browser',
		'methodToTransfer'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
TransferMethodApplier class >> applyFrom: aCodeProvider [

	aCodeProvider selectedMessageName ifNotNil: [ :selectedSelector |
		(self 
			on: aCodeProvider 
			for: aCodeProvider selectedClassOrMetaClass>>selectedSelector) value ].
	
]

{ #category : 'instance creation' }
TransferMethodApplier class >> on: aBrowser for: aMethodToTransfer [

	^self new initializeOn: aBrowser for: aMethodToTransfer 
]

{ #category : 'refactoring - creation' }
TransferMethodApplier >> createRefactoring [

	^self refactoringClass for: methodToTransfer.
]

{ #category : 'refactoring - changes' }
TransferMethodApplier >> informChangesToBrowser [

	browser selector: refactoring selector refactoredTo: nil
]

{ #category : 'initialization' }
TransferMethodApplier >> initializeOn: aBrowser for: aMethodToTransfer [

	browser := aBrowser.
	methodToTransfer := aMethodToTransfer.
]

{ #category : 'refactoring - creation' }
TransferMethodApplier >> refactoringClass [

	self subclassResponsibility 
]

{ #category : 'refactoring - parameters request' }
TransferMethodApplier >> requestRefactoringParameters [

	
]

{ #category : 'refactoring - changes' }
TransferMethodApplier >> showChanges [

	self informChangesToBrowser.
]
