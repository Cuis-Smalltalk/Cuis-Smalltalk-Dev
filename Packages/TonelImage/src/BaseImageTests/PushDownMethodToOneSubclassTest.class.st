Class {
	#name : 'PushDownMethodToOneSubclassTest',
	#superclass : 'PushDownMethodTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test01MethodShouldPushedDownOnlyToSelectedSubclass [

	| classToPushDownTo sibling classToRefactor push |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString.
           
	self 
		assertCreation: [ push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo ]
		warnsWith: [ PushDownMethodToOneSubclass warningMessageNotARefactoringBecauseNotImplementedIn: { sibling } ].
	push apply.
   
	self assert: (classToPushDownTo includesSelector: self selectorToPushDown).
	self deny: (sibling includesSelector: self selectorToPushDown).
	self deny: (classToRefactor includesSelector: self selectorToPushDown).
	
	
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test02MethodShouldPushedDownIfVarNotShadowedInClassToPushDownTo [

	| classToPushDownTo sibling classToRefactor instVar push |
	
	instVar := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor instanceVariableNames: instVar.
	
	classToRefactor compile: self selectorToPushDown, '|', instVar, '|' classified: self methodToPushDownCategory.
	sibling compile: self selectorToPushDown asString.
           
	self
		shouldnt: [ push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo ]
		raise: self refactoringError.
	push apply.
	
	self assert: (classToPushDownTo includesSelector: self selectorToPushDown).
	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test03MethodShouldNotPushedDownIfVarShadowedInClassToPushDownTo [

	| classToPushDownTo classToRefactor instVar |
	
	instVar := 'a'.
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor instanceVariableNames: instVar.
	
	classToRefactor compile: self selectorToPushDown, '|', instVar, '|'.
	   
	self
		assertCreation: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo ]
		failsWith: [ PushDownMethodToOneSubclass errorMessageCanNotPushDownWithShadowedInstVarsOf:
			{classToPushDownTo -> {instVar }} asDictionary].
	
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test04ExistingMethodInSubclassShouldBeOverriden [
	
	| classToRefactor classToPushDownMethodTo methodToPushDownSourceCode push |
	
	methodToPushDownSourceCode := self selectorToPushDown, ' ^ 2'.

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactor compile: methodToPushDownSourceCode.
	 
	classToPushDownMethodTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	classToPushDownMethodTo compile: self selectorToPushDown, ' ^ 1'.

	self 
		assertCreation: [ push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownMethodTo ]
		warnsWith: [ PushDownMethodToOneSubclass warningMesssageForMessageAlreadyImplementedIn: { classToPushDownMethodTo } ].
	push apply.
		
	self assert: (classToPushDownMethodTo includesSelector: self selectorToPushDown).
	self deny: (classToRefactor includesSelector: self selectorToPushDown).
	self assert: methodToPushDownSourceCode equals: (classToPushDownMethodTo >> self selectorToPushDown) sourceCode
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test05AMethodCannotBePushedToClassThatIsNotInHierarchy [

	| classNotInRefactorHierarchy classToRefactor |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classNotInRefactorHierarchy := self createClassNamed: self classToPushDownTo.
	
	classToRefactor compile: self selectorToPushDown asString.
         
	self 
		assertCreation: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classNotInRefactorHierarchy] 
		failsWith: [ PushDownMethodToOneSubclass errorMessage: classNotInRefactorHierarchy isNotSubclassOf: classToRefactor ].
	
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test06WarnWhenMethodNotImplementedInSiblings [

	| classToPushDownTo classToRefactor sibling |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString.
         
	self 
		assertCreation: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo] 
		warnsWith: [ PushDownMethodToOneSubclass warningMessageNotARefactoringBecauseNotImplementedIn: { sibling } ].
	
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test07ShouldNotWarnWhenMethodImplementedInSiblings [

	| classToPushDownTo classToRefactor sibling |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString.
 	sibling compile: self selectorToPushDown asString.
         
	self 
		shouldnt: [ PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo] 
		raise: self refactoringWarning 
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test08ShouldImplementAsSubclassResponsibilityWhenImplementedInAllSubclasses [

	| classToPushDownTo classToRefactor sibling push expectedClassToRefactorSourceCode |

	classToRefactor := self createClassNamed: self classToRefactorName.
	classToPushDownTo := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	sibling := self createClassNamed: self classToPushDownTo2 subclassOf: classToRefactor.
	
	classToRefactor compile: self selectorToPushDown asString classified: self methodToPushDownCategory.
 	sibling compile: self selectorToPushDown asString.
         
	push := PushDownMethodToOneSubclass for: classToRefactor >> self selectorToPushDown to: classToPushDownTo.
	push apply.
	
	expectedClassToRefactorSourceCode := String streamContents: [ :stream |
		stream nextPutAll: self selectorToPushDown; newLine; newLine; tab; nextPutAll: 'self '; nextPutAll: #subclassResponsibility asString ].

	self assert: classToRefactor implementsSendingSubclassResponsibility: self selectorToPushDown.
	self assert: expectedClassToRefactorSourceCode equals: (classToRefactor >> self selectorToPushDown) sourceCode.
]

{ #category : 'tests' }
PushDownMethodToOneSubclassTest >> test09AClassMethodShouldBePushedDownToSubclasses [

	| classToRefactorSubclass classToRefactor push |
   
	classToRefactor := self createClassNamed: self classToRefactorName.
	classToRefactorSubclass := self createClassNamed: self classToPushDownTo subclassOf: classToRefactor.
	
	classToRefactor class compile: self selectorToPushDown classified: self methodToPushDownCategory.
           
	push := PushDownMethodToOneSubclass for: classToRefactor class >> self selectorToPushDown to: classToRefactorSubclass class.
	push apply.
   
	self deny: (classToRefactor class includesSelector: self selectorToPushDown).
	self assert: (classToRefactorSubclass class includesSelector: self selectorToPushDown).
	
]
