Class {
	#name : 'MoveToInstanceOrClassMethod',
	#superclass : 'TransferMethod',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
MoveToInstanceOrClassMethod class >> assertIsNotAccessingInstanceVariable: aMethodToMove [
	
	aMethodToMove methodClass instVarNames 
		do: [ :instVarName | 
			(aMethodToMove accessesInstanceVariable: instVarName) ifTrue: [self signalMethodCannotAccessInstanceVariable]].
]

{ #category : 'pre-conditions' }
MoveToInstanceOrClassMethod class >> assertLocalVariableDoesNotConflictWithInstanceVariable: aMethodToMove [

	| newClass |

	aMethodToMove methodClass isMeta
		ifTrue: [newClass := aMethodToMove methodClass soleInstance]
		ifFalse: [newClass := aMethodToMove methodClass class].

	newClass instVarNames
		do: [ :instVarName |
			(aMethodToMove notOptimizedMethodNode hasLocalNamed: instVarName)
				ifTrue: [self signalLocalVariableConflictsWithInstanceVariable]
			]
]

{ #category : 'instance creation' }
MoveToInstanceOrClassMethod class >> for: aMethodToMove [

	self assertIsNotAccessingInstanceVariable: aMethodToMove.
	self assertLocalVariableDoesNotConflictWithInstanceVariable: aMethodToMove.
	
	^self new initializeFor: aMethodToMove 
]

{ #category : 'exceptions' }
MoveToInstanceOrClassMethod class >> localVariableConflictsWithInstanceVariableErrorMessage [
	
	^ 'Can not move a method that uses a local variable with same name as an instance variable'
]

{ #category : 'exceptions' }
MoveToInstanceOrClassMethod class >> referencingInstanceVariablesErrorMessage [
	
	^ 'Can not move a method that accesses an instance variable'
]

{ #category : 'exceptions' }
MoveToInstanceOrClassMethod class >> signalLocalVariableConflictsWithInstanceVariable [
	
	self refactoringError: self localVariableConflictsWithInstanceVariableErrorMessage
]

{ #category : 'exceptions' }
MoveToInstanceOrClassMethod class >> signalMethodCannotAccessInstanceVariable [
	
	self refactoringError: self referencingInstanceVariablesErrorMessage
]

{ #category : 'applying - private' }
MoveToInstanceOrClassMethod >> moveMethod [

	| targetClass |

	targetClass := method methodClass isMeta
		ifTrue: [method methodClass soleInstance]
		ifFalse: [method methodClass class].

	targetClass
		compile: method sourceCode
		classified: self methodCategory.

	
]
