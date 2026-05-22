Class {
	#name : 'PushUpMethodTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
PushUpMethodTest >> classToRefactorName	 [

	^#ClassToPushMethodUp
]

{ #category : 'tests' }
PushUpMethodTest >> test01AnEmptyMethodOnClassToRefactorShouldBePushedUpToSuperclass [

	| classToRefactorSuperclass classToRefactor selector push |

	selector := #newMethod.
   
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector asString.
           
	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
	
	
]

{ #category : 'tests' }
PushUpMethodTest >> test02AMethodReadingInstVarWarnsWhenPushedUpToSuperclass [

	| classToRefactorSuperclass classToRefactor selector newVariable refactoring |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, '^ 1 + ', newVariable.
	
	self
		assertCreation: [ refactoring := PushUpMethod for: classToRefactor >> selector ]
		 warnsWith: [ PushUpMethod accessingInstanceVariableWarningMessage: { newVariable }].
		
	refactoring apply.
	
	self assert: (classToRefactorSuperclass definesInstanceVariableNamed: newVariable).
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor definesInstanceVariableNamed: newVariable).
	self deny: (classToRefactor includesSelector: selector).
]

{ #category : 'tests' }
PushUpMethodTest >> test03AMethodWritingInstVarWarnsWhenPushedUpToSuperclass [

	| classToRefactorSuperclass classToRefactor selector newVariable refactoring |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, ' a = 1'.

	self
		assertCreation: [ refactoring := PushUpMethod for: classToRefactor >> selector ]
		warnsWith: [ PushUpMethod accessingInstanceVariableWarningMessage: { newVariable }].
		
	refactoring apply.
	
	self assert: (classToRefactorSuperclass definesInstanceVariableNamed: newVariable).
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor definesInstanceVariableNamed: newVariable).
	self deny: (classToRefactor includesSelector: selector).
]

{ #category : 'tests' }
PushUpMethodTest >> test04AMethodWritesSuperClassInstVarShouldBePushedUpToSuperclass [

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' a = 1'.	

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
]

{ #category : 'tests' }
PushUpMethodTest >> test05AMethodAccessesSuperClassClassToRefactorInstVarShouldBePushedUpToSuperclass [

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, '^ 1 + ', newVariable.

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
	   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
]

{ #category : 'tests' }
PushUpMethodTest >> test06AMethodAccessesSuperClassClassToRefactorInstVarShouldBePushedUpToSuperclass [

	| classToRefactorSuperclass classToRefactor selector newVariable push |

	newVariable := 'a'.	
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, '^ 1 + ', newVariable.

	push := PushUpMethod for: classToRefactor >> selector.
	push apply.
	   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
]

{ #category : 'tests' }
PushUpMethodTest >> test07ShouldWarnWhenNotEquivalentMethodExistsInSuperclass [
	
	| classToRefactor classToRefactorSuperclass newVariable selector superclassMethodContent |
	
	newVariable := 'a'.	
	selector := #newMethod.
	superclassMethodContent := selector, '
	^ 2 + ', newVariable, '.'.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.
	classToRefactorSuperclass compile: selector, '^ 1 + ', newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: superclassMethodContent.

	self 
		assertCreation: [ PushUpMethod for: classToRefactor >> selector ]
		warnsWith: [ PushUpMethod methodWithDifferentImplementationInSuperclass: { selector } ].
		
	
]

{ #category : 'tests' }
PushUpMethodTest >> test08RemovesEquivalentMethodsFromSiblings [

	| classToRefactorSuperclass classToRefactor selector push sourceCode classToRefactorSibling |

	selector := #newMethod.
   	sourceCode := selector, ' ^10 factorial'.
	
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'Sibling') asSymbol subclassOf: classToRefactorSuperclass.
	classToRefactorSibling compile: sourceCode.
           
	push := PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector  -> {classToRefactorSibling} } asDictionary pushingUp: Set new.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self deny: (classToRefactor includesSelector: selector).
	self deny: (classToRefactorSibling includesSelector: selector).
	
	
]

{ #category : 'tests' }
PushUpMethodTest >> test09CannotRemoveNotImplementedMethodFromAdditionalSubclasses [

	| classToRefactorSuperclass classToRefactor selector sourceCode classToRefactorSibling |

	selector := #newMethod.
   	sourceCode := selector, ' ^10 factorial'.
	
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'Sibling') asSymbol subclassOf: classToRefactorSuperclass.
	   
	self 
		assertCreation: [  PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector -> {classToRefactorSibling} } asDictionary pushingUp: Set new ]
		failsWith: [ PushUpMethod errorMessageForMethodCannotBeRemovedMethodFrom: { classToRefactorSibling } ].

	
	
	
]

{ #category : 'tests' }
PushUpMethodTest >> test10CannotRemoveMethodFromNoSiblings [

	| classToRefactorSuperclass classToRefactor selector sourceCode classToRefactorSibling |

	selector := #newMethod.
   	sourceCode := selector, ' ^10 factorial'.
	
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'NoSibling') asSymbol subclassOf: self defaultSuperclass.
	classToRefactorSibling compile: sourceCode.
    
	self 
		assertCreation: [  PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector -> { classToRefactorSibling } } asDictionary pushingUp: Set new ]
		failsWith: [ PushUpMethod errorMessageForNoSiblings: { classToRefactorSibling } ].

	
	
	
]

{ #category : 'tests' }
PushUpMethodTest >> test11CannotRemoveNotEquivalantMethodsFromSiblings [

	| classToRefactorSuperclass classToRefactor selector classToRefactorSibling |

	selector := #newMethod.
   	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^10 factorial'.
 	classToRefactorSibling := self createClassNamed: (self classToRefactorName,'NoSibling') asSymbol subclassOf: classToRefactorSuperclass.
	classToRefactorSibling compile: selector, ' ^11 factorial'.
    
	self 
		assertCreation: [  PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: { classToRefactor >> selector  -> {classToRefactorSibling} } asDictionary pushingUp: Set new ]
		failsWith: [ PushUpMethod errorMessageForNotEquivalentMethods: { classToRefactorSibling } ].

	
	
	
]

{ #category : 'tests' }
PushUpMethodTest >> test12ShouldNotWarnWhenEquivalentMethodExistsInSuperclass [
	
	| classToRefactor classToRefactorSuperclass selector sourceCode |
	
	selector := #newMethod.
	sourceCode := selector, ' ^10 factorial'.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: sourceCode.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: sourceCode.

	self 
		shouldnt: [ PushUpMethod for: classToRefactor >> selector ]
		raise: self refactoringWarning 
	
]

{ #category : 'tests' }
PushUpMethodTest >> test13FailsWhenAccessingInstVarDefinedAsTemporaryInSuperclass [

	| classToRefactorSuperclass classToRefactor selector superclassSelector newVariable |

	newVariable := 'a'.	
	superclassSelector := #superClassNewMethod.
	selector := #newMethod.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: superclassSelector, '|', newVariable, '|', '^ 1 + ', newVariable.
	
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, '^ 1 + ', newVariable.
	
	self
		assertCreation: [ PushUpMethod for: classToRefactor >> selector ]
		failsWith: [ PushUpInstanceVariable errorMessageForInstanceVariable: newVariable isDefinedInMethodsOf: classToRefactorSuperclass ].
]

{ #category : 'tests' }
PushUpMethodTest >> test14ShouldNotWarnWhenMethodExistsInSuperclassIsSubclassResponsibility [
	
	| classToRefactor classToRefactorSuperclass selector superclassMethodContent |
	
	selector := #newMethod.
	superclassMethodContent := selector, ' self ', #subclassResponsibility.
	 
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: superclassMethodContent.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^1'.

	self 
		shouldnt: [ PushUpMethod for: classToRefactor >> selector ]
		raise: self refactoringWarning 
	
	
]

{ #category : 'tests' }
PushUpMethodTest >> test15AMethodThatDependsOnOtherMethodPushesUpMethodsToSuperclass [

	| classToRefactorSuperclass classToRefactor selector dependantSelector refactoring |

	selector := #newMethod.
	dependantSelector := #dependantMethod.

	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: dependantSelector, ' ^ 1'.

	refactoring := PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector } asSet.
	refactoring apply.

	self assert: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactorSuperclass includesSelector: dependantSelector).
	self deny: (classToRefactor includesSelector: selector).
	self deny: (classToRefactor includesSelector: dependantSelector).
]

{ #category : 'tests' }
PushUpMethodTest >> test16AMethodThatDependsOnACycleOfMethodsPushesUpMethodsToSuperclass [

	| classToRefactorSuperclass classToRefactor selector dependantSelector refactoring otherDependantSelector |

	selector := #newMethod.
	dependantSelector := #dependantMethod.
	otherDependantSelector := #otherDependantSelector.

	classToRefactorSuperclass := self createClassNamed: #Superclass.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: dependantSelector, ' ^ self ', otherDependantSelector.
	classToRefactor compile: otherDependantSelector, ' ^ self ', selector.

	refactoring := PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector . classToRefactor >> otherDependantSelector } asSet.
	refactoring apply.

	self assert: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactorSuperclass includesSelector: dependantSelector).
	self assert: (classToRefactorSuperclass includesSelector: otherDependantSelector).
	self deny: (classToRefactor includesSelector: selector).
	self deny: (classToRefactor includesSelector: dependantSelector).
	self deny: (classToRefactor includesSelector: otherDependantSelector).
]

{ #category : 'tests' }
PushUpMethodTest >> test17FailsWhenPushingADependantMethodAccessingInstVarDefinedAsTemporaryInSuperclass [

	| classToRefactorSuperclass classToRefactor selector superclassSelector dependantSelector newVariable |

	newVariable := 'a'.	
	superclassSelector := #superClassNewMethod.
	selector := #newMethod.
	dependantSelector := #dependantMethod.

	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: superclassSelector, '|', newVariable, '|', '^ 1 + ', newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass instanceVariableNames: newVariable.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: dependantSelector, ' ^ 1 + ', newVariable.

	self
		assertCreation: [ PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector } asSet ]
		failsWith: [ PushUpInstanceVariable errorMessageForInstanceVariable: newVariable isDefinedInMethodsOf: classToRefactorSuperclass ].
]

{ #category : 'tests' }
PushUpMethodTest >> test18ShouldWarnWhenNotEquivalentMethodOfDependantMethodExistsInSuperclass [

	| classToRefactor classToRefactorSuperclass newVariable selector dependantSelector superclassMethodContent |

	newVariable := 'a'.	
	selector := #newMethod.
	dependantSelector := #dependantMethod.
	superclassMethodContent := dependantSelector, '
	^ 2 + ', newVariable, '.'.

	classToRefactorSuperclass := self createClassNamed: #Superclass instanceVariableNames: newVariable.
	classToRefactorSuperclass compile: dependantSelector, '^ 1 + ', newVariable.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector, ' ^ self ', dependantSelector.
	classToRefactor compile: superclassMethodContent.

	self 
		assertCreation: [ PushUpMethod for: classToRefactor >> selector additionallyRemovingFrom: Dictionary new pushingUp: { classToRefactor >> dependantSelector } asSet ]
		warnsWith: [ PushUpMethod methodWithDifferentImplementationInSuperclass: { dependantSelector } ].
]

{ #category : 'tests' }
PushUpMethodTest >> test19ShouldSelectSiblingClassesWithEquivalentMethod [

	| superclass classToTest classWithEquivalentMethod selector methodContent equivalentMethod|

	selector := #newMethod.
	methodContent :=  selector, '^ 2'.

	superclass := self createClassNamed: #Superclass.

	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	
	classWithEquivalentMethod := self createClassNamed: #ClassWithEquivalentMethod subclassOf: superclass.
	classWithEquivalentMethod compile: methodContent.
	
	equivalentMethod := PushUpMethod equivalentMethodsOfSiblingsOfAll: { classToTest >> selector }.
	   
	self assert: (equivalentMethod size = 1).
	self assert: (equivalentMethod includesKey: (classToTest >> selector)).
	self assert: ((equivalentMethod at: classToTest >> selector) = { classWithEquivalentMethod }).
]

{ #category : 'tests' }
PushUpMethodTest >> test20ShouldNotSelectSiblingClassesWithNoEquivalentMethod [

	| superclass classToTest classWithEquivalentMethod selector  equivalentMethod|

	selector := #newMethod.

	superclass := self createClassNamed: #Superclass.

	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: selector, '^ 2'.
	
	classWithEquivalentMethod := self createClassNamed: #ClassWithEquivalentMethod subclassOf: superclass.
	classWithEquivalentMethod compile: selector, '^ 3'.
	
	equivalentMethod := PushUpMethod equivalentMethodsOfSiblingsOfAll: { classToTest >> selector }.
	   
	self assert: (equivalentMethod isEmpty).
]

{ #category : 'tests' }
PushUpMethodTest >> test21WhenMultipleMethodsShouldSelectSiblingClassesWithEquivalentMethodForEachOne [

	| superclass classToTest classWithEquivalentMethod selector anotherSelector methodContent anotherMethodContent equivalentMethod|

	selector := #newMethod.
	anotherSelector := #newMethod2.
	methodContent :=  selector, '^ 2'.
	anotherMethodContent := anotherSelector, '^ 1'.

	superclass := self createClassNamed: #Superclass.

	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	classToTest compile: anotherMethodContent.
	
	classWithEquivalentMethod := self createClassNamed: #ClassWithEquivalentMethod subclassOf: superclass.
	classWithEquivalentMethod compile: methodContent.
	classWithEquivalentMethod compile: anotherMethodContent.
	
	equivalentMethod := PushUpMethod equivalentMethodsOfSiblingsOfAll: { classToTest >> selector . classToTest >> anotherSelector }.
	   
	self assert: (equivalentMethod size = 2).
	self assert: (equivalentMethod includesKey: (classToTest >> selector)).
	self assert: (equivalentMethod includesKey: (classToTest >> anotherSelector)).
	self assert: ((equivalentMethod at: classToTest >> selector) = { classWithEquivalentMethod }).
	self assert: ((equivalentMethod at: classToTest >> anotherSelector) = { classWithEquivalentMethod }).
]

{ #category : 'tests' }
PushUpMethodTest >> test22ShouldSelectDependantSelectors [

	| superclass classToTest selector dependantSelector methodContent dependantMethodContent dependantSelectors|

	selector := #newMethod.
	dependantSelector := #dependantMethod.
	methodContent :=  selector, '^ self ', dependantSelector.
	dependantMethodContent :=  dependantSelector, '^ 2'.

	superclass := self createClassNamed: #Superclass.
	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	classToTest compile: dependantMethodContent.
	
	dependantSelectors := PushUpMethod selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: ( classToTest >> selector ).
	   
	self assert: (dependantSelectors size = 1).
	self assert: (dependantSelectors includes: dependantSelector).
]

{ #category : 'tests' }
PushUpMethodTest >> test23ShouldNotSelectDependantSelectorsWhenThereIsNone [

	| superclass classToTest selector methodContent dependantSelectors|

	selector := #newMethod.
	methodContent :=  selector, '^ 2'.

	superclass := self createClassNamed: #Superclass.
	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	
	dependantSelectors := PushUpMethod selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: ( classToTest >> selector ).
	   
	self assert: (dependantSelectors isEmpty).
]

{ #category : 'tests' }
PushUpMethodTest >> test24ShouldSelectCyclicDependantSelectors [

	| superclass classToTest selector dependantSelector otherDependantSelector methodContent dependantMethodContent otherDependantMethodContent dependantSelectors|

	selector := #newMethod.
	dependantSelector := #dependantMethod.
	otherDependantSelector := #otherDependantSelector.
	methodContent :=  selector, '^ self ', dependantSelector.
	dependantMethodContent :=  dependantSelector, '^ self ', otherDependantSelector.
	otherDependantMethodContent := otherDependantSelector, '^ self ', selector.

	superclass := self createClassNamed: #Superclass.
	classToTest := self createClassNamed: #ClassToTest subclassOf: superclass.
	classToTest compile: methodContent.
	classToTest compile: dependantMethodContent.
	classToTest compile: otherDependantMethodContent.
	
	dependantSelectors := PushUpMethod selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: ( classToTest >> selector ).
	   
	self assert: (dependantSelectors size = 2).
	self assert: (dependantSelectors includes: dependantSelector).
	self assert: (dependantSelectors includes: otherDependantSelector).
]
