Class {
	#name : 'RemoveAllUnreferencedInstanceVariablesTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'tests' }
RemoveAllUnreferencedInstanceVariablesTest >> test01NoVariableIsRemovedWhenClassHasNoInstanceVariable [

	| classToRefactor refactoring removedInstanceVariables |
	
	classToRefactor := self createClassNamed: #ClassWithoutInstVar.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: removedInstanceVariables isEmpty
]

{ #category : 'tests' }
RemoveAllUnreferencedInstanceVariablesTest >> test02UnreferencedVariablesAreRemoved [

	| classToRefactor refactoring removedInstanceVariables unreferencedVariable |
	
	unreferencedVariable := 'a'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: unreferencedVariable.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariable)
	
]

{ #category : 'tests' }
RemoveAllUnreferencedInstanceVariablesTest >> test03VariablesWithReferencesAreNotRemoved [

	| classToRefactor refactoring removedInstanceVariables unreferencedVariableName referencedVariableName |
	
	referencedVariableName := 'a'.
	unreferencedVariableName := 'b'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: referencedVariableName, ' ', unreferencedVariableName.
	classToRefactor compile: 'm1 ^', referencedVariableName.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariableName).
	
	

]

{ #category : 'tests' }
RemoveAllUnreferencedInstanceVariablesTest >> test04VariablesWithReferencesInSubclassesAreNotRemoved [

	| classToRefactor refactoring removedInstanceVariables unreferencedVariableName referencedVariableName classToRefactorSubclass |
	
	referencedVariableName := 'a'.
	unreferencedVariableName := 'b'.
	classToRefactor := self createClassNamed: #ClassWithInstVar instanceVariableNames: referencedVariableName, ' ', unreferencedVariableName.
	classToRefactorSubclass := self createClassNamed: #ClassWithReferenceInstVar subclassOf: classToRefactor.
	classToRefactorSubclass compile: 'm1 ^', referencedVariableName.
	
	refactoring := RemoveAllUnreferencedInstanceVariables from: classToRefactor.
	removedInstanceVariables := refactoring apply.
	
	self assert: 1 equals: removedInstanceVariables size.
	self assert: (removedInstanceVariables includes: unreferencedVariableName).
	

]
