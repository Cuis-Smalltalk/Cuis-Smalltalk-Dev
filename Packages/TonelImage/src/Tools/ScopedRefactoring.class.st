Class {
	#name : 'ScopedRefactoring',
	#superclass : 'Refactoring',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aPotentialClassToRefactor [

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor theNonMetaClass.
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor theMetaClass.


]

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization [

	| categories |

	categories := Set new.

	self
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inHierarchyOf: aClass
		doingPerClass: [:aClassInHierarchy | categories add: aClassInHierarchy category ].

	categories do: [:aCategory |
		self
			addImplementorsOf: anOldSelector
			to: implementors
			andSendersTo: senders
			inCategory: aCategory
			organizedBy: anOrganization ].

]

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization [

	| classesInCategory rootCategory |

	rootCategory := anOrganization rootCategoryOf: aCategory.
	classesInCategory := anOrganization allClassesAt: rootCategory.
	classesInCategory do: [ :aPotentialClassToRefactor |
		self
			addImplementorsOf: anOldSelector
			to: implementors
			andSendersTo: senders
			forClassAndMetaOf: aPotentialClassToRefactor ].

]

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass [

	self
		addImplementorsOf: anOldSelector
		to: implementors
		andSendersTo: senders
		inHierarchyOf: aClass
		doingPerClass: [ :aClassInHierarchy | ]

	
]

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass doingPerClass: aBlock [

	| highestClassImplementingOldSelector |

	highestClassImplementingOldSelector := aClass highestClassImplementing: anOldSelector ifNone: [ aClass ].
	highestClassImplementingOldSelector theNonMetaClass withAllSubclassesDo: [ :aPotentialClassToRefactor |
		aPotentialClassToRefactor isMeta ifFalse: [
			self
				addImplementorsOf: anOldSelector
				to: implementors
				andSendersTo: senders
				forClassAndMetaOf: aPotentialClassToRefactor.
			aBlock value: aPotentialClassToRefactor ]]
	
]

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem [

	aSystem allBehaviorsDo: [ :aPotentialClassToRefactor |
		self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor ].

]

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addImplementorsOf: anOldSelector to: implementors andSendersTo: senders of: aPotentialClassToRefactor [

	| potentialImplementor |

	"Phil B. requested to avoid refactoring OMeta2 classes, so right now
	it avoids implementors and senders whose compilerClass is not register
	as allowed compiler - Hernan"
	(self canRefactor: aPotentialClassToRefactor) ifFalse: [ ^self ].

	potentialImplementor := aPotentialClassToRefactor compiledMethodAt: anOldSelector ifAbsent: nil.
	potentialImplementor ifNotNil: [ implementors add: potentialImplementor ].

	(aPotentialClassToRefactor whichSelectorsReferTo: anOldSelector) do: [ :aSelector |
		self addTo: senders referencesTo: anOldSelector from: aSelector in: aPotentialClassToRefactor ]
]

{ #category : 'implementors and senders' }
ScopedRefactoring class >> addTo: senders referencesTo: anOldSelector from: aSelector in: aPotentialClassToRefactor [

	senders add: (MethodReference class: aPotentialClassToRefactor selector: aSelector)
]
