'From Cuis7.3 [latest update: #7082] on 20 March 2025 at 2:10:21 pm'!

!ScopedRefactoring class methodsFor: 'implementors and senders' stamp: 'HAW 3/20/2025 14:00:46'!
addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aCategory organizedBy: anOrganization

	| classesInCategory rootCategory |

	rootCategory := (aCategory findTokens: '-') first.
	classesInCategory := anOrganization allClassesAt: rootCategory.
	classesInCategory do: [ :aPotentialClassToRefactor |
		self
			addImplementorsOf: anOldSelector
			to: implementors
			andSendersTo: senders
			forClassAndMetaOf: aPotentialClassToRefactor ].
! !


!ScopedRefactoringApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/20/2025 14:08:17'!
basicScopeOptionLabels

	"Do not inline/rename/remove. Used by LiveTyping - Hernan"

	^`{'In Class'. 'In Hierarchy'. 'In Class Category Tree'. 'In Hierarchy and its Categories Tree'. 'In System'}`! !

