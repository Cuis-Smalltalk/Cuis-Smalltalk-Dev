Class {
	#name : 'RenameInstanceVariableTest',
	#superclass : 'RefactoringTest',
	#instVars : [
		'oldVariable',
		'newVariable',
		'classToRefactor'
	],
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'assertions' }
RenameInstanceVariableTest >> assertRenameCreationFailsWith: aStringOrBlock [

	self
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: aStringOrBlock 
]

{ #category : 'class factory' }
RenameInstanceVariableTest >> classToRefactorName [

	^#ClassToRenameInstVar
]

{ #category : 'class factory' }
RenameInstanceVariableTest >> createClassToRefactor [
	
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: oldVariable 
]

{ #category : 'class factory' }
RenameInstanceVariableTest >> createClassToRefactorAndAssertRenameCreationFailsWith: aStringOrBlock [

	self createClassToRefactor.
	self assertRenameCreationFailsWith: aStringOrBlock 
]

{ #category : 'setup' }
RenameInstanceVariableTest >> setUp [

	super setUp.
	
	oldVariable := 'old'.
	newVariable := 'new'.
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test01CanNotRenameAnInstanceVariableNotDefinedInClass [

	classToRefactor := self createClassNamed: #ClassWithoutInstVar instanceVariableNames: ''.
	
	self assertRenameCreationFailsWith: [ RenameInstanceVariable errorMessageForInstanceVariable: oldVariable notDefinedIn: classToRefactor ]
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test02NewVariableNameCanNotBeEmpty [
	
	newVariable := '   '.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition newVariableCanNotBeEmptyErrorMessage ]

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test03NewVariableNameCanNotContainBlanks [
	
	newVariable := 'a b'.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable]

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test04NewVariableNameCanNotContainInvalidCharacters [

	newVariable := '2a'.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForInvalidInstanceVariable: newVariable]

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test05ValidNewVariableNameGetBlanksTrimmed [

	newVariable := ' a '.
	self createClassToRefactor.
			
	self
		shouldnt: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		raise: self refactoringError 
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test06NewVariableNameCanNotBeDefinedInClass [

	newVariable := oldVariable.
	self createClassToRefactorAndAssertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactor )]
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test07NewVariableNameCanNotBeDefinedInSuperclasses [

	|  classToRefactorSuperclass  |

	classToRefactorSuperclass := self createClassNamed: #SuperclassWithInstVar instanceVariableNames: newVariable.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: oldVariable.
	
	self assertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSuperclass)]
	
	

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test08NewVariableNameCanNotBeDefinedInAnySubclass [

	| classToRefactorSubclass |

	self createClassToRefactor.
	classToRefactorSubclass := self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	self assertRenameCreationFailsWith: [NewInstanceVariablePrecondition errorMessageForNewInstanceVariable: newVariable alreadyDefinedInAll: (Array with: classToRefactorSubclass)]
	
		
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test09RenameCreatesNewInstanceVariableAndDeletesOldOne [

	| rename |

	self createClassToRefactor.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	self assert: (classToRefactor definesInstanceVariableNamed: newVariable).
	self deny: (classToRefactor definesInstanceVariableNamed: oldVariable) 

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test10ReadReferencesToOldVariableAreRenamed [

	| selector method rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactor compile: selector , ' ^' , oldVariable.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method readsInstanceVariable: newVariable).
	self deny: (method readsInstanceVariable: oldVariable) 

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test11WriteReferencesToOldVariableAreRenamed [

	|  selector method rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactor compile: selector , ' ' , oldVariable, ' := 10'.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method writesInstanceVariable: newVariable).
	self deny: (method writesInstanceVariable: oldVariable) 

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test12ReferencesInSubclassesAreRenamed [

	| selector method classToRefactorSubclass rename |

	selector := #m1.
	self createClassToRefactor.
	classToRefactorSubclass _ self createClassNamed: #SubclassWithInstVar subclassOf: classToRefactor instanceVariableNames: ''.
	classToRefactorSubclass compile: selector , ' ' , oldVariable, ' := 10. ^' , oldVariable.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactorSubclass >> selector.
	self assert: (method readsInstanceVariable: newVariable).
	self assert: (method writesInstanceVariable: newVariable).
	self deny: (method readsInstanceVariable: oldVariable).
	self deny: (method writesInstanceVariable: oldVariable) 
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test13ReferencesToOtherVariablesAreNotRenamed [

	| variableToKeep selector method rename |

	variableToKeep := 'keep'.
	selector := #m1.
	classToRefactor := self createClassNamed:  self classToRefactorName instanceVariableNames: oldVariable, ' ', variableToKeep.
	classToRefactor compile: selector , ' ' , variableToKeep, ' := ' , oldVariable, '. ^' , variableToKeep.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor.
	rename apply.

	method := classToRefactor >> selector.
	self assert: (method readsInstanceVariable: variableToKeep).
	self assert: (method writesInstanceVariable: variableToKeep) 

]

{ #category : 'tests' }
RenameInstanceVariableTest >> test14NewVariableOfExistingInstancesReferToOldVariableValue [

	| rename instance |

	self createClassToRefactor.
	instance := classToRefactor new.
	instance instVarNamed: oldVariable put: 1.
	
	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	self assert: 1 equals:  (instance instVarNamed: newVariable).
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test15FailsWhenNewVariableEqualsTemporaryVariableOfAMethodInClass [

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' | ', newVariable, ' | ', newVariable, ' := 10.'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test16FailsWhenNewVariableEqualsArgumentOfAMethodInClass [

	| selector |

	self createClassToRefactor.
	selector := #m1:.
	classToRefactor compile: selector, newVariable.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test17FailsWhenNewVariableEqualsArgumentOfABlockInAMethodInClass [

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' [ :', newVariable, ' | ] value: 1'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test18FailsWhenNewVariableEqualsTemporaryOfABlockInAMethodInClass [

	| selector |

	self createClassToRefactor.
	selector := #m1.
	classToRefactor compile: selector, ' [ | ', newVariable, ' | ] value'.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (classToRefactor>>selector)) ].
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test19FailsWhenNewVariableIsHiddenInAnyMethodOfAnySubclass [

	| selector subclass |

	self createClassToRefactor.
	subclass := self createClassNamed: #SubclassOfClassToRefactor subclassOf: classToRefactor.
	selector := #m1.
	subclass compile: selector, ' | ', newVariable, ' | '.
	
	self 
		assertCreation: [ RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor ]
		failsWith: [ NewInstanceVariablePrecondition errorMessageForNewVariable: newVariable willBeHiddenAtAll: (Array with: (subclass>>selector)) ].
	
]

{ #category : 'tests' }
RenameInstanceVariableTest >> test20ClassChangeIsAddedWhenRenamingAnInstanceVariable [

	| rename classChanges refactorClassChangeSet refactorClassChangeSets package |

	"I have to do this because on this case I want to keep track of the changes - Hernan"
	classToRefactor := self 
		createClassNamed: self classToRefactorName 
		subclassOf: Object 
		instanceVariableNames: oldVariable 
		classVariableNames: ''
		poolDictionaries: ''
		category: self classCategoryOfTestData.

	rename := RenameInstanceVariable from: oldVariable to: newVariable in: classToRefactor .
	rename apply.
	
	refactorClassChangeSets := ChangeSet allChangeSetsWithClass: classToRefactor.
	self assert: refactorClassChangeSets size equals: 1.
	
	refactorClassChangeSet := refactorClassChangeSets anyOne.	
	classChanges := refactorClassChangeSet classChangeAt: classToRefactor name.
	self assert: (classChanges includes: #change).
	
	ChangeSet allChangeSets remove: refactorClassChangeSet.
	package := CodePackage packageOfClass: classToRefactor ifNone: nil.
	self assert: package notNil.
	package hasUnsavedChanges: false 
]
