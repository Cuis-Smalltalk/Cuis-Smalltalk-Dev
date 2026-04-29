'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 12:55:08 pm'!

!CodeFileBrowser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 12:53:15'!
basicClassList
	"Answer an empty array if no selection exists."
	(selectedSystemCategory isNil or: [ caseCodeSource isNil ])
		ifTrue: [ ^#() ].

	^ caseCodeSource classes
		sorted: [:a :b | a name <= b name]! !


!CodeFileBrowser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 12:50:59'!
hierarchicalClassList
	"Order the pseudoclasses filed-in lexicographically. Then order them hierarchically. Answer an empty array if no selection exists."

	| pseudoClasses |
	(selectedSystemCategory isNil or: [ caseCodeSource isNil ])
		ifTrue: [ ^#() ].

	pseudoClasses := caseCodeSource classes sorted: [:firstPseudoClass :secondPseudoClass | firstPseudoClass name < secondPseudoClass name].
	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: pseudoClasses)
		on: OrderedCollection new
		indent: ''.! !

!methodRemoval: CodeFileBrowser #classList stamp: 'FJG 4/Apr/2026 12:53:34'!
CodeFileBrowser removeSelector: #classList!
