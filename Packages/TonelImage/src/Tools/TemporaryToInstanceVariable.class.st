Class {
	#name : 'TemporaryToInstanceVariable',
	#superclass : 'Refactoring',
	#instVars : [
		'variable',
		'method'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'assertions' }
TemporaryToInstanceVariable class >> assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName [

	| counter |

	counter := TemporaryVariableDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: counter.
	
	counter count = 1 ifFalse: [
		self refactoringError: self inexistentTemporaryErrorDescription ].
]

{ #category : 'assertions' }
TemporaryToInstanceVariable class >> assertNoOtherBlockIn: aMethodNode definesTemporaryNamed: aTemporaryVariableName [

	| temporaryCounter argumentCounter |

	temporaryCounter := TemporaryVariableDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: temporaryCounter.

	argumentCounter := ArgumentDeclarationCounter for: aTemporaryVariableName.
	aMethodNode accept: argumentCounter.

	temporaryCounter count + argumentCounter count > 1 ifTrue: [
		self refactoringError: self temporaryExistsInOtherBlockErrorDescription ].
	
]

{ #category : 'assertions' }
TemporaryToInstanceVariable class >> assertNoOtherMethodIn: aClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName [

 	| methodsDefiningTemporaryInHierarchy |

	methodsDefiningTemporaryInHierarchy := OrderedCollection new.
	aClass withAllSubclassesDo: [ :subclass | 
		methodsDefiningTemporaryInHierarchy addAll: (subclass methodsWithArgumentOrTemporaryNamed: aTemporaryVariableName) ].

	methodsDefiningTemporaryInHierarchy size > 1 ifTrue: [
		self 
			canNotRefactorDueToReferencesError: self temporaryExistsInOtherMethodsErrorDescription
			references: (methodsDefiningTemporaryInHierarchy collect: [ :implementor | MethodReference method: implementor ])
			to: aTemporaryVariableName. ].
]

{ #category : 'assertions' }
TemporaryToInstanceVariable class >> assertSubclassesOf: aClass haventGotInstanceVariableNamed: anInstanceVariableName [
	
	aClass allSubclassesDo: [ :subclass | 
		(subclass instVarNames includes: anInstanceVariableName) ifTrue: [
			self refactoringError: self temporaryExistsAsInstVarInSubclassesErrorDescription ]].
]

{ #category : 'error descriptions' }
TemporaryToInstanceVariable class >> inexistentTemporaryErrorDescription [

	^'The temporary variable does not exist.'
]

{ #category : 'instance creation' }
TemporaryToInstanceVariable class >> named: aTemporaryVariableName fromMethod: aMethodNode [

	| methodClass |

	methodClass := aMethodNode methodClass.
	
	self assertNoOtherBlockIn: aMethodNode definesTemporaryNamed: aTemporaryVariableName.
	self assert: aMethodNode hasTemporaryNamed: aTemporaryVariableName.
	self assertNoOtherMethodIn: methodClass orSubclassesDefinesTemporaryNamed: aTemporaryVariableName. 
	self assertSubclassesOf: methodClass haventGotInstanceVariableNamed: aTemporaryVariableName.
	
	^self new initializeNamed: aTemporaryVariableName fromMethod: aMethodNode.
]

{ #category : 'error descriptions' }
TemporaryToInstanceVariable class >> temporaryExistsAsInstVarInSubclassesErrorDescription [

	^'The temporary variable exists as an instance variable in a subclass.'
]

{ #category : 'error descriptions' }
TemporaryToInstanceVariable class >> temporaryExistsInOtherBlockErrorDescription [

	^'The temporary exists in other blocks in this method; remove those first.'.
]

{ #category : 'error descriptions' }
TemporaryToInstanceVariable class >> temporaryExistsInOtherMethodsErrorDescription [

	^'Temporary variable exists in other methods; remove those first.'
]

{ #category : 'applying' }
TemporaryToInstanceVariable >> addInstanceVariable [

	(AddInstanceVariable named: variable to: method methodClass) apply.
]

{ #category : 'applying' }
TemporaryToInstanceVariable >> apply [

	| newSourceCode |

	newSourceCode := self removeTemporary.
	self addInstanceVariable.
	
	^newSourceCode.
]

{ #category : 'initialization' }
TemporaryToInstanceVariable >> initializeNamed: aTemporaryVariableName fromMethod: aMethodNode  [
	variable := aTemporaryVariableName.
	method := aMethodNode.
]

{ #category : 'applying' }
TemporaryToInstanceVariable >> removeTemporary [

	| remover |
	
	remover := TemporaryVariableDeclarationRemover in: method for: variable. 
	method accept: remover.
	^remover newSourceCode. 
	
]
