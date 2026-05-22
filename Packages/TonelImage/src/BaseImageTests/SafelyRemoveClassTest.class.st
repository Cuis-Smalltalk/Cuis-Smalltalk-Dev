Class {
	#name : 'SafelyRemoveClassTest',
	#superclass : 'RefactoringTest',
	#category : 'BaseImageTests-Tools-Refactoring',
	#package : 'BaseImageTests',
	#tag : 'Tools-Refactoring'
}

{ #category : 'test support' }
SafelyRemoveClassTest >> assertSubclassesReturnsACopy [

	"This is a precondition for the remove to work properly with subclasses of the class to remove. 
	I do not put it in the refactoring to avoid innecesary checks and because tests verify preconditions. 
	It is not a seprate test because I do not want senders of this precondition to run if it fails - Hernan"
	self deny: self class superclass subclasses == self class superclass subclasses
]

{ #category : 'class factory' }
SafelyRemoveClassTest >> classToRemoveName [

	"I can not use the symbol directly because it would be a reference - Hernan"
	^ 'ClassToRemove' asSymbol.
	
]

{ #category : 'test support' }
SafelyRemoveClassTest >> safelyRemoveHierarchyOf: classToRemove [
		
	| remove |
	
	"see comment of #assertSubclassesReturnsACopy - Hernan"
	self assertSubclassesReturnsACopy.
	
	self 
		assertCreation: [ remove := SafelyRemoveClass of: classToRemove ]
		warnsWith: [ SafelyRemoveClass warningMessageFor: classToRemove hasSubclasses: classToRemove allSubclasses ].
		
	remove apply.
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test02ClassesWithNoReferencesAndNoSubclassesAreSafetelyRemoved [

	| classToRemove safeRemove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	safeRemove := SafelyRemoveClass of: classToRemove.
	safeRemove apply.
	
	self assert: classToRemove isObsolete 
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test03RemovingTheMetaclassRemovesTheClass [

	| classToRemove safeRemove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	safeRemove := SafelyRemoveClass of: classToRemove class.
	safeRemove apply.
	
	self assert: classToRemove isObsolete 
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test04CanNotRemoveClassWithReferencesOutsideHierarchy [

	| classToRemove classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemove.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^', classToRemove name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self canNotRefactorDueToReferencesRefactoringError 
		withExceptionDo: [ :anError | | reference |
			self 
				assert: (SafelyRemoveClass errorMessageForCanNotRemove: classToRemove dueToReferencesToAll: (Array with: classToRemove))
				equals: anError messageText.
			self assert: 1 equals: anError numberOfReferences.
			reference := anError anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test05CanRemoveClassWithReferencesFromItself [

	| classToRemove methodNameReferencingClass remove | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	methodNameReferencingClass := #m1.
	classToRemove compile: methodNameReferencingClass asString, ' ^', classToRemove name asString.
	
	remove := SafelyRemoveClass of: classToRemove.
	remove apply.
	
	self assert: classToRemove isObsolete 
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test06WarnIfClassToRemoveHasSubclasses [

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	
	self 
		assertCreation: [ SafelyRemoveClass of: classToRemove ]
		warnsWith: [ SafelyRemoveClass warningMessageFor: classToRemove hasSubclasses: (Array with: classToRemoveSubclass) ]
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test07CanNotRemoveClassWhenSubclassesHaveReferencesOutsideTheHierarchy [

	| classToRemove classToRemoveSubclass classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemoveSubclass.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^', classToRemoveSubclass name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self canNotRefactorDueToReferencesRefactoringError 
		withExceptionDo: [ :anError | | reference |
			self 
				assert: (SafelyRemoveClass errorMessageForCanNotRemove: classToRemove dueToReferencesToAll: (Array with: classToRemoveSubclass))
				equals: anError messageText.
			self assert: 1 equals: anError numberOfReferences.
			reference := anError anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]	

]

{ #category : 'tests' }
SafelyRemoveClassTest >> test08HierarchyIsRemovedIfSubclassesWarningIsResumed [

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	
	self safelyRemoveHierarchyOf: classToRemove.
	
	self assert: classToRemove isObsolete.
	self assert: classToRemoveSubclass isObsolete
		
	
		
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test09CanRemoveIfReferencesToSubclassesAreInTheHierarchy [

	| classToRemove classToRemoveSubclass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classToRemoveSubclass := self createClassNamed: 'ClassToRemoveSubclass' asSymbol subclassOf: classToRemove.
	classToRemove compile: 'm1 ^', classToRemoveSubclass name asString.
	classToRemoveSubclass compile: 'm2 ^', classToRemove name asString.
	
	self safelyRemoveHierarchyOf: classToRemove.
	
	self assert: classToRemove isObsolete.	
	self assert: classToRemoveSubclass isObsolete.
]

{ #category : 'tests' }
SafelyRemoveClassTest >> test10WarnWhenHasReferencesToName [

	| classToRemove classReferencing methodNameReferencingClass | 
	
	classToRemove := self createClassNamed: self classToRemoveName.
	classReferencing := self createClassNamed: #ClassReferencingClassToRemove.
	methodNameReferencingClass := #m1.
	classReferencing compile: methodNameReferencingClass asString, ' ^#', classToRemove name asString.
	
	self
		should: [ SafelyRemoveClass of: classToRemove ]
		raise: self referencesRefactoringWarning 
		withExceptionDo: [ :aWarning | | reference |
			self 
				assert: (SafelyRemoveClass warningMessageForReferencesToNames: (Array with: classToRemove))
				equals: aWarning messageText.
			self assert: 1 equals: aWarning numberOfReferences.
			reference := aWarning anyReference.
			self assert: classReferencing name equals: reference classSymbol.
			self assert: methodNameReferencingClass equals: reference methodSymbol ]
]
