Class {
	#name : 'ScopedRefactoringApplier',
	#superclass : 'RefactoringApplier',
	#instVars : [
		'scopeChoice',
		'selectedClass'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'instance creation' }
ScopedRefactoringApplier class >> applyFrom: aCodeProvider [

	self subclassResponsibility
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> askScope [

	| scopeMenu |

	scopeMenu := PopUpMenu labelArray: self scopeOptionLabels.
	scopeChoice := scopeMenu startUpWithCaption: 'Select Refactoring Scope'.
	scopeChoice = 0 ifTrue: [ self endRequest ].
	
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> basicCalculateImplementorsAndSenders [

  "Do not inline/rename/remove. Used by LiveTyping - Hernan"

	scopeChoice = 1 ifTrue: [ ^self implementorsAndSendersForClass ].
	scopeChoice = 2 ifTrue: [ ^self implementorsAndSendersForHierarchy ].
	scopeChoice = 3 ifTrue: [ ^self implementorsAndSendersInCategory ].
	scopeChoice = 4 ifTrue: [ ^self implementorsAndSendersInCategoryAndHierarchy ].
	scopeChoice = 5 ifTrue: [ ^self implementorsAndSendersInSystem ].

	self error: 'Unknown scope option'

		
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> basicScopeOptionLabels [

	"Do not inline/rename/remove. Used by LiveTyping - Hernan"

	^`{'In Class'. 'In Hierarchy'. 'In Class Category Tree'. 'In Hierarchy and its Categories Tree'. 'In System'}`
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> calculateImplementorsAndSenders [

	"Do not inline this message send, #basicCalculateImplementorsAndSenders
	is used in live typing - Hernan"
	self basicCalculateImplementorsAndSenders
		
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> collectionToAddSendersTo [

	self subclassResponsibility
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> collectionToAddimplementorsTo [

	self subclassResponsibility
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> implementorsAndSendersForClass [

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		forClassAndMetaOf: selectedClass
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> implementorsAndSendersForHierarchy [

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inHierarchyOf: selectedClass
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> implementorsAndSendersInCategory [

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inCategory: selectedClass category
		organizedBy: SystemOrganization
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> implementorsAndSendersInCategoryAndHierarchy [

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inCategoriesAndHierarchyOf: selectedClass
		organizedBy: SystemOrganization 
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> implementorsAndSendersInSystem [

	^self refactoringClass
		addImplementorsOf: self selectorToLookImplementorsFor
		to: self collectionToAddimplementorsTo
		andSendersTo: self collectionToAddSendersTo
		inSystem: Smalltalk 
]

{ #category : 'refactoring - creation' }
ScopedRefactoringApplier >> refactoringClass [

	self subclassResponsibility
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> scopeOptionLabels [

	"Do not inline #basicScopeOptionLabels it is used by
	live typing - Hernan"
	^self basicScopeOptionLabels 
]

{ #category : 'refactoring - parameters request' }
ScopedRefactoringApplier >> selectorToLookImplementorsFor [

	self subclassResponsibility 
]
