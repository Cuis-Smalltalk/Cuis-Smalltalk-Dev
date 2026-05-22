Class {
	#name : 'MoveToInstanceOrClassMethodTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
MoveToInstanceOrClassMethodTest >> classToRefactorName [

	^#ClassToMoveMethod
]

{ #category : 'tests' }
MoveToInstanceOrClassMethodTest >> test01CannotMoveWhenReferencingInstanceVariables [
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: referencedVariable.
	classToRefactor compile: selector asString,' ^', referencedVariable.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod referencingInstanceVariablesErrorMessage ]

]

{ #category : 'tests' }
MoveToInstanceOrClassMethodTest >> test02CannotMoveToClassWhenLocalVariableIsTheSameAsClassInstanceVariable [
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor class addInstVarName: referencedVariable.
	classToRefactor compile: selector asString, '|', referencedVariable, '|'.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod localVariableConflictsWithInstanceVariableErrorMessage ]

]

{ #category : 'tests' }
MoveToInstanceOrClassMethodTest >> test03CannotMoveToInstanceWhenLocalVariableIsTheSameAsClassInstanceVariable [
	
	| classToRefactor selector referencedVariable |

	selector := #newMethod.
	referencedVariable := 'a'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor addInstVarName: referencedVariable.
	classToRefactor class compile: selector asString, '|', referencedVariable, '|'.
	
	self 
		assertCreation: [ MoveToInstanceOrClassMethod for: classToRefactor class >> selector ]
		failsWith: [ MoveToInstanceOrClassMethod localVariableConflictsWithInstanceVariableErrorMessage ]

]

{ #category : 'tests' }
MoveToInstanceOrClassMethodTest >> test04InstanceMethodIsMovedToClassMethod [
	
	| classToRefactor selector moveMethod |

	selector := #newMethod.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: selector asString.
	
	moveMethod := MoveToInstanceOrClassMethod for: classToRefactor >> selector.
	moveMethod apply.
	
	self assert: (classToRefactor class includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).

]

{ #category : 'tests' }
MoveToInstanceOrClassMethodTest >> test05ClassMethodIsMovedToInstanceMethod [
	
	| classToRefactor selector moveMethod |

	selector := #newMethod.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor class compile: selector asString.
	
	moveMethod := MoveToInstanceOrClassMethod for: classToRefactor class >> selector.
	moveMethod apply.
	
	self assert: (classToRefactor includesSelector: selector).
	self deny: (classToRefactor class includesSelector: selector).

]
