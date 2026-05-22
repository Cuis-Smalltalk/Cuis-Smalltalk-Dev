Class {
	#name : 'PushDownMethodToSubclassesTest',
	#superclass : 'PushDownMethodTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test01AnEmptyMethodOnClassToRefactorShouldBePushedDownToSubclasses [

	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor push |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.

	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).
	
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test02CanNotPushDownWhenATemporaryVarIsAnInstVarOnASubclass [
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable |
	
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	classToRefactor compile: self selectorToPushDown, '|', newVariable, '|'.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToRefactorSubclass2 -> {newVariable }} asDictionary ].
	   
	self deny: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self deny: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).
	self assert: (classToRefactor includesSelector: self selectorToPushDown).
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test03CanNotPushDownWhenAnArgumentIsAnInstVarOnASubclass [
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable selectorToPushDown |
	
	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor instanceVariableNames: newVariable.
	
	selectorToPushDown := #newMethod:.
	classToRefactor compile: selectorToPushDown, newVariable.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToRefactorSubclass2 -> {newVariable }} asDictionary].
	   
	self deny: (classToRefactorSubclass1 includesSelector: selectorToPushDown).
	self deny: (classToRefactorSubclass2 includesSelector: selectorToPushDown).
	self assert: (classToRefactor includesSelector: selectorToPushDown).
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test04AMethodAccessesClassToRefactorInstVarShouldBePushedDownToSubclasses [
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable push |

	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown, '^ 1 + ', newVariable classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.

	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test05AMethodWritesClassToRefactorInstVarShouldBePushedDownToSubclasses [
	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor newVariable push |

	newVariable := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown, ' a = 1' classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.
   
	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 includesSelector: self selectorToPushDown).
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test06AMethodInClassToRefactorExistsInSubClassesShouldOverrideTheExistingSubClassesMethod [
	
	| classToRefactor classToRefactorSubclass newVariable subclassMethodContent |
	
	newVariable := 'a'.	
	subclassMethodContent := self selectorToPushDown, '
	^ 2 + ', newVariable, '.'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactor compile: subclassMethodContent.
	 
	classToRefactorSubclass := self createClassNamed: #Subclass subclassOf: classToRefactor.
	classToRefactorSubclass compile: self selectorToPushDown, '^ 1 + ', newVariable.

	self 
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown ]
		warnsWith: [ PushDownMethodToSubclasses warningMesssageForMessageAlreadyImplementedIn: { classToRefactorSubclass } ].
		
	self assert: (classToRefactorSubclass includesSelector: self selectorToPushDown).
	self assert: (classToRefactor includesSelector: self selectorToPushDown).
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test07AMethodInClassToRefactorShouldBeReplacedBySubclassResponsibility [

	| classToRefactor push  expectedBody selectorToPushDown selectorAndArguments |

	classToRefactor := self createClassNamed: self classToRefactorName.
	self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.

	selectorToPushDown := #newMethod:.
	selectorAndArguments := selectorToPushDown, ' anABC'.
	classToRefactor compile: selectorAndArguments classified: self methodToPushDownCategory.

	push := PushDownMethodToSubclasses for: classToRefactor >> selectorToPushDown.
	push apply.

	expectedBody := String streamContents: [ :stream |
		stream nextPutAll: selectorAndArguments ; newLine; newLine; tab; nextPutAll: 'self '; nextPutAll: #subclassResponsibility asString ].

	self assert: classToRefactor implementsSendingSubclassResponsibility: selectorToPushDown.
	self assert: expectedBody equals: (classToRefactor >> selectorToPushDown) sourceCode. 
'newMethod: anABC
 
 	self subclassResponsibility' 
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test08AMethodInClassToRefactorExistsInSubClassesShouldNotOverrideTheExistingSubClassesMethodIfEquivalent [

	| classToRefactor classToRefactorSubclass newVariable subclassMethodContent push |

	newVariable := 'a'.	
	subclassMethodContent := self selectorToPushDown, '
	^ 2 + ', newVariable, '.'.

	classToRefactor := self createClassNamed: self classToRefactorName instanceVariableNames: newVariable.
	classToRefactor compile: subclassMethodContent.

	classToRefactorSubclass := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass compile: subclassMethodContent.

	push := PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown.
	push apply.

	self assert: (classToRefactorSubclass includesSelector: self selectorToPushDown).
	self assert: subclassMethodContent equals: (classToRefactorSubclass >> self selectorToPushDown) getSource.
	self assert: (classToRefactor includesSelector: self selectorToPushDown).
	self deny: (subclassMethodContent = (classToRefactor >> self selectorToPushDown) getSource).
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test09CannotPushDownMethodWithoutSubclasses [

	| classToRefactor |
	
	classToRefactor := self createClassNamed: self classToRefactorName.
	
	classToRefactor compile: self selectorToPushDown.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor >> self selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageNoSubclassToPushDownMethodTo ].
	
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test10AClassMethodShouldBePushedDownToSubclasses [

	| classToRefactorSubclass1 classToRefactorSubclass2 classToRefactor push |
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass1 := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToRefactorSubclass2 := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor class compile: self selectorToPushDown classified: self methodToPushDownCategory.
           
	push := PushDownMethodToSubclasses for: classToRefactor class >> self selectorToPushDown.
	push apply.
   
	self assert: classToRefactor class implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: (classToRefactorSubclass1 class includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass2 class includesSelector: self selectorToPushDown).
]

{ #category : 'tests' }
PushDownMethodToSubclassesTest >> test11CannotPushDownClassMethodWithoutSubclasses [

	| classToRefactor |
	
	classToRefactor := self createClassNamed: self classToRefactorName.

	classToRefactor class compile: self selectorToPushDown.
           
	self
		assertCreation: [ PushDownMethodToSubclasses for: classToRefactor class >> self selectorToPushDown ]
		 failsWith: [ PushDownMethodToSubclasses errorMessageNoSubclassToPushDownMethodTo ].
]
