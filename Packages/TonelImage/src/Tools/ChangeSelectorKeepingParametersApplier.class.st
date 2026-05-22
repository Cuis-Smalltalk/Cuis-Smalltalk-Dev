Class {
	#name : 'ChangeSelectorKeepingParametersApplier',
	#superclass : 'ChangeSelectorApplier',
	#instVars : [
		'newSelector'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'refactoring - parameters request' }
ChangeSelectorKeepingParametersApplier >> askNewSelector [

	| enteredString |

	enteredString := self request: 'Enter new selector:' initialAnswer: oldSelector.
	newSelector := enteredString withBlanksTrimmed asSymbol.


]

{ #category : 'refactoring - parameters request' }
ChangeSelectorKeepingParametersApplier >> assertCanRenameSelector [

	self refactoringClass assertIsValidToRenameFrom: oldSelector to: newSelector.
	
]

{ #category : 'refactoring - creation' }
ChangeSelectorKeepingParametersApplier >> createRefactoring [

	^self refactoringClass from: oldSelector to: newSelector implementors: implementors senders: senders.
	
]

{ #category : 'refactoring - parameters request' }
ChangeSelectorKeepingParametersApplier >> requestRefactoringParameters [

	self
		askNewSelector;
		assertCanRenameSelector
		
]
