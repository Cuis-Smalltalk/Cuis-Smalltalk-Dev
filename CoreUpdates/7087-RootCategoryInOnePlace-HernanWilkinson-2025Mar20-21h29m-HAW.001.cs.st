'From Cuis7.3 [latest update: #7084] on 20 March 2025 at 9:42:35 pm'!

!SystemOrganizer methodsFor: 'categories' stamp: 'HAW 3/20/2025 21:34:22'!
rootCategoryOf: aCategory

	^ (aCategory findTokens: '-') first! !


!Browser methodsFor: 'system category list' stamp: 'HAW 3/20/2025 21:37:23'!
systemCategoryRoots
	"Answer the roots of the forest of system categories."
	| categories subcategories roots name |
	categories := self systemCategoryList.
	subcategories := Dictionary new.
	categories do: [:each|
		name := SystemOrganization rootCategoryOf: each.
		(subcategories at: name ifAbsentPut: [OrderedCollection new]) add: each].
	roots := OrderedCollection new.
	categories do: [:each|
		name := SystemOrganization rootCategoryOf: each.
		(roots includes: name) ifFalse:
			[(subcategories at: name) size > 1 ifTrue: [roots add: name] ifFalse: [roots add: (subcategories at: name) first]]].
	^ roots collect: [:each| SystemCategoryWrapper with: each name: each model: self]! !


!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/20/2025 21:36:26'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization

	| classesInCategory rootCategory |

	rootCategory := anOrganization rootCategoryOf: aCategory.
	classesInCategory := anOrganization allClassesAt: rootCategory.
	classesInCategory do: [ :aPotentialClassToRefactor |
		self
			addImplementorsOf: anOldSelector
			to: implementors
			andSendersTo: senders
			forClassAndMetaOf: aPotentialClassToRefactor ].
! !


!TestSuite class methodsFor: 'instance creation' stamp: 'HAW 3/20/2025 21:38:29'!
forSystemCategoryTreeOf: aCategoryName  using: aSystemOrganizer

	| rootCategory |

	rootCategory := aSystemOrganizer rootCategoryOf: aCategoryName.
	
	^self forSystemCategoryNamed: rootCategory using: aSystemOrganizer ! !

!methodRemoval: ScopedRefactoring class #rootCategoryOf: stamp: 'HAW 3/20/2025 21:35:48'!
ScopedRefactoring class removeSelector: #rootCategoryOf:!

!SystemOrganizer reorganize!
('fileIn/Out' fileOutAllCategories fileOutCategory: fileOutCategory:on:initializing: fileOutCategoryNoPoolsNoInit:on: objectForDataStream: superclassOrderIn:)
('remove' categoriesMatching: removeEmptySystemCategories removeMissingClasses removeSystemCategories: removeSystemCategory:)
('categories' allClassesAt: classesAt: fullListAtCategoryNamed: hasCategory: renameSystemCategory:to: rootCategoryOf: subCategoriesOf: subCategoriesPatternOf: superclassOrderInAll: testCaseClassesAt: withSubCategoriesOf:)
('source code management' classMethodCountOf: instanceMethodCountOf: linesOfCodeOf: messageSendsCountOf:)
!


!ScopedRefactoring class reorganize!
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: addImplementorsOf:to:andSendersTo:inSystem: addImplementorsOf:to:andSendersTo:of: addTo:referencesTo:from:in:)
!

