Class {
	#name : 'AddAsSubclassResponsabilityTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'class factory' }
AddAsSubclassResponsabilityTest >> classToRefactorName	 [

	^#ClassToAddMethodInSuperClassAsSubclassResponsibility
]

{ #category : 'tests' }
AddAsSubclassResponsabilityTest >> test01AMethodOnClassToRefactorShouldBeAddedAsSubclassResponsibilityInSuperclass [

	| classToRefactorSuperclass classToRefactor selector push methodCategory |

	selector := #newMethod.
   
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	methodCategory := 'xyz'.
	classToRefactor compile: selector asString classified: methodCategory. 
           
	push := AddAsSubclassResponsibility for: classToRefactor >> selector.
	push apply.
   
	self assert: (classToRefactorSuperclass includesSelector: selector).
	self assert: (classToRefactor includesSelector: selector).
	self assert: methodCategory equals: (classToRefactorSuperclass organization categoryOfElement: selector)
]

{ #category : 'tests' }
AddAsSubclassResponsabilityTest >> test02ShouldWarnWhenMethodWithSameNameExistsInSuperclass [
	
	| classToRefactor classToRefactorSuperclass selector |
	
	selector := #newMethod.
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: selector, '^ 1 '.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector asString.

	self 
		assertCreation: [ AddAsSubclassResponsibility for: classToRefactor >> selector ]
		warnsWith: [ AddAsSubclassResponsibility methodImplementedInSuperclassWarningMessage ].
		
	
]

{ #category : 'tests' }
AddAsSubclassResponsabilityTest >> test03ShouldNotWarnWhenMethodInSuperclassIsAbstract [
	
	| classToRefactor classToRefactorSuperclass selector |
	
	selector := #newMethod.
	classToRefactorSuperclass := self createClassNamed: #Superclass.
	classToRefactorSuperclass compile: selector, ' self ', #subclassResponsibility.

	classToRefactor := self createClassNamed: self classToRefactorName subclassOf: classToRefactorSuperclass.
	classToRefactor compile: selector.

	self 
		shouldnt: [ AddAsSubclassResponsibility for: classToRefactor >> selector ]
		raise: self refactoringWarning 
	
]
