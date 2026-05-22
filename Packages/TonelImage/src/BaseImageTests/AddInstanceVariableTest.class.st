Class {
	#name : 'AddInstanceVariableTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'newVariable'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
AddInstanceVariableTest >> classToRefactorName [

	^#ClassToAddInstVar
]

{ #category : 'tests' }
AddInstanceVariableTest >> test01NewVariableNameCanNotBeEmpty [
	
	newVariable := '   '.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition newVariableCanNotBeEmptyErrorMessage ]

]

{ #category : 'tests' }
AddInstanceVariableTest >> test02NewVariableNameCanNotContainBlanks [
	
	newVariable := 'a b'.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable ]

	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test03NewVariableNameCanNotContainInvalidCharacters [

	newVariable := '2a'.
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: self class ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable ]


]

{ #category : 'tests' }
AddInstanceVariableTest >> test04ValidNewVariableNameGetBlanksTrimmed [

	newVariable := ' a '.
			
	self assert: newVariable withBlanksTrimmed equals: (AddInstanceVariable named: newVariable to: self class) newVariable
]

{ #category : 'tests' }
AddInstanceVariableTest >> test05NewVariableNameCanNotBeDefinedInClass [

	| classToRefactor |
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactor )]
	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test06NewVariableNameCanNotBeDefinedInSuperclasses [

	|  classToRefactorSuperclass classToRefactor  |

	newVariable := 'a'.
	classToRefactorSuperclass := self createClassNamed: #SuperclassWithInstVar instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSuperclass)]

]

{ #category : 'tests' }
AddInstanceVariableTest >> test07NewVariableNameCanNotBeDefinedInAnySubclass [

	| classToRefactor classToRefactorSubclass |
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass := self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	self 
		assertCreation: [AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSubclass)]
	
		
]

{ #category : 'tests' }
AddInstanceVariableTest >> test08AddCreatesNewInstanceVariable [

	| classToRefactor add | 
	
	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.

	add := AddInstanceVariable named: newVariable to: classToRefactor.
	add apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: newVariable).
	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test09FailsWhenNewVariableEqualsTemporaryVariableOfAMethodInClass [

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' | ', newVariable, ' | ', newVariable, ' := 10.'.
	
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test10FailsWhenNewVariableEqualsArgumentOfAMethodInClass [

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1:.
	classToRefactor compile: selector, newVariable.
	
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test11FailsWhenNewVariableEqualsArgumentOfABlockInAMethodInClass [

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' [ :', newVariable, ' | ] value: 1'.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test12FailsWhenNewVariableEqualsTemporaryOfABlockInAMethodInClass [

	| selector classToRefactor |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	selector := #m1.
	classToRefactor compile: selector, ' [ | ', newVariable, ' | ] value'.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test13FailsWhenNewVariableIsHiddenInAnyMethodOfAnySubclass [

	| selector classToRefactor subclass |

	newVariable := 'a'.
	classToRefactor := self createClassNamed: self classToRefactorName.
	subclass := self createClassNamed: #SubclassOfClassToRefactor subclassOf: classToRefactor.
	selector := #m1.
	subclass compile: selector, ' | ', newVariable, ' | '.
		
	self 
		assertCreation: [ AddInstanceVariable named: newVariable to: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (subclass>>selector)) ].
	
	
]

{ #category : 'tests' }
AddInstanceVariableTest >> test14NewVariableNameCanNotBeAReservedName [

	| classToRefactor |

	classToRefactor := self createClassNamed: self classToRefactorName.
	
	ClassBuilder reservedNames do: [ :aReservedName | 
		self 
			assertCreation: [AddInstanceVariable named: aReservedName to: classToRefactor ]
			failsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariableCanNotBeAReservedName: aReservedName ]]
		
]
