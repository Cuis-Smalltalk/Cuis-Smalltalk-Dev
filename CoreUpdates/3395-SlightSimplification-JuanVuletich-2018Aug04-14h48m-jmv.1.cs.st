'From Cuis 5.0 of 7 November 2016 [latest update: #3394] on 4 August 2018 at 2:48:52 pm'!

!Browser methodsFor: 'class list' stamp: 'jmv 8/4/2018 14:48:21'!
classListIndexOf: aClassNameToFind

	"Answer the index of the aClassName selection."

	aClassNameToFind ifNil: [ ^0 ].
	^self classList findFirst: [ :showingClassName |
		"Works regardless of currently showing hierarchically or alphabetically."
		showingClassName afterBlanksEndsWith: aClassNameToFind  ]! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'jmv 8/4/2018 14:48:30'!
selectClass: classNotMeta
	
	self classListIndex: (self classListIndexOf: classNotMeta name)! !

!methodRemoval: Browser #classListIndexWhenShowingHierarchicallyOf:!
Browser removeSelector: #classListIndexWhenShowingHierarchicallyOf:!
