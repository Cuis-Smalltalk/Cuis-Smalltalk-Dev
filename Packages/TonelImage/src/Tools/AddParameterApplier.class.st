Class {
	#name : 'AddParameterApplier',
	#superclass : 'ChangeSelectorApplier',
	#instVars : [
		'newParameter',
		'newParameterValue',
		'newKeyword',
		'parameterIndex'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
AddParameterApplier class >> assertCanApplyRefactoringFor: aSelector in: aSelectedClass [

	AddParameter assertCanAddParameterTo: aSelector.


	
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> addAsLastParameterLabel [

	^ 'Add as last parameter'
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> askInsertionIndex [

	| methodNode originalMethod parameterNames |

	"See RemoveParameterApplier#askParameterToRemove to understand why I ask for the index using
	the keywords when no method is found - Hernan"
	originalMethod := selectedClass
		compiledMethodAt: oldSelector
		ifAbsent: [ ^self askInsertionIndexUsingKeywords ].

	methodNode := originalMethod notOptimizedMethodNode.
	parameterNames := methodNode argumentNames.
	parameterNames add: self addAsLastParameterLabel.

	parameterIndex := (PopUpMenu labelArray: parameterNames) startUpWithCaption: 'Add parameter before?'.
	parameterIndex = 0 ifTrue: [self endRequest ].

		
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> askInsertionIndexIfNecessary [

	oldSelector isKeyword ifTrue: [ self askInsertionIndex ].
	
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> askInsertionIndexUsingKeywords [

	| keywords |

	keywords := oldSelector keywords asOrderedCollection.
	keywords add: self addAsLastParameterLabel.

	parameterIndex := (PopUpMenu labelArray: keywords) startUpWithCaption: 'Select keyword to add parameter before'.
	parameterIndex = 0 ifTrue: [self endRequest ].

	
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> askNewKeyword [

 	| enteredString |
	
	enteredString := (self request: 'Enter keyword for new parameter') withBlanksTrimmed.
	(enteredString endsWith: ':') ifFalse: [ enteredString := enteredString, ':' ].
	newKeyword := enteredString asSymbol.
	self refactoringClass assertIsValidKeywordForNewParameter: newKeyword
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> askNewKeywordIfNecessary [
		
	oldSelector isKeyword ifTrue: [self askNewKeyword]
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> askNewParameter [
		
	| enteredString |
	
	enteredString := self request: 'Enter new parameter name'.
	newParameter := enteredString withBlanksTrimmed.
	self refactoringClass assertIsValidParameterName: newParameter

]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> askNewParameterValue [
		
	| enteredString |
	
	enteredString := self request: 'Enter parameter value for senders'.
	newParameterValue := enteredString withBlanksTrimmed.
	self refactoringClass assertNewParameterValueIsValid: newParameterValue.

]

{ #category : 'refactoring - creation' }
AddParameterApplier >> createRefactoring [
	
	oldSelector isUnary ifTrue: [ ^self createRefactoringForUnarySelector].
	oldSelector isKeyword ifTrue: [ ^self createRefactoringForKeywordSelector ].
	
	self error: 'oldSelector should be unary or keyword!'
]

{ #category : 'refactoring - creation' }
AddParameterApplier >> createRefactoringForKeywordSelector [
	
	^self refactoringClass 
		named: newParameter
		at: parameterIndex 
		initializedWith: newParameterValue 
		using: newKeyword 
		toKeywordSelector: oldSelector 
		implementors: implementors 
		senders: senders 
]

{ #category : 'refactoring - creation' }
AddParameterApplier >> createRefactoringForUnarySelector [
	
	^self refactoringClass 
		named: newParameter
		initializedWith: newParameterValue 
		toUnarySelector: oldSelector 
		implementors: implementors 
		senders: senders 
]

{ #category : 'refactoring - creation' }
AddParameterApplier >> refactoringClass [

	^AddParameter
]

{ #category : 'refactoring - parameters request' }
AddParameterApplier >> requestRefactoringParameters [

	self
		askNewParameter;
		askNewParameterValue;
		askInsertionIndexIfNecessary;
		askNewKeywordIfNecessary
		
]
