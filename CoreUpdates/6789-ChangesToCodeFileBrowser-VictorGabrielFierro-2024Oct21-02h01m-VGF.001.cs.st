'From Cuis7.1 [latest update: #6787] on 21 October 2024 at 2:05:30 am'!

!Browser methodsFor: 'class list' stamp: 'VGF 10/21/2024 02:01:14'!
createHierarchyTreeOf: classes comparingWith: aBlockForComparingClasses
	"Create a tree from a collection of classes. Keep ordering while doing it.
	The Each node in the tree is an Association with a class as key and a collection
	of child nodes as value. The first argument of the comparing block is the potential superclass 
	and the second the potential subclass."

	| classesWithSubclassIndexes |

	"For each class, collect the indexes of its included subclasses."
	classesWithSubclassIndexes := classes collect: [ :eachClass | 
		| subclasses subclassIndexes |
		subclasses := classes select: [ :class | aBlockForComparingClasses value: eachClass value: class].
		subclassIndexes := subclasses collect: [ :child | classes indexOf: child ].
		Association key: eachClass value: subclassIndexes ].

	"Now replace the indexes by the actual Associations, forming a tree.
	Remove them from the root collection."
	classesWithSubclassIndexes copy do: [ :classAndSubclassIndexes |
		classAndSubclassIndexes value: (classAndSubclassIndexes value collect: [ :idx | 
			| subclassAssociation |
			subclassAssociation := classesWithSubclassIndexes at: idx.
			classesWithSubclassIndexes at: idx put: nil.
			subclassAssociation ])].

	^ classesWithSubclassIndexes select: [ :ea | ea notNil ].
! !


!CodeFileBrowser methodsFor: 'class list' stamp: 'VGF 10/21/2024 02:03:22'!
createHierarchyTreeOf: pseudoClasses 
	"Create a tree from a collection of pseudoclasses"
	
	^self createHierarchyTreeOf: pseudoClasses comparingWith: [:aPotentialSuperClass 			:aPotentialSubClass | aPotentialSuperClass name = aPotentialSubClass superclassName].! !

!CodeFileBrowser methodsFor: 'class list' stamp: 'VGF 10/21/2024 02:03:02'!
hierarchicalClassList
	"Order the pseudoclasses filed-in lexicographically. Then order them hierarchically."

	| pseudoClasses |
	pseudoClasses := caseCodeSource classes sorted: [:firstPseudoClass :secondPseudoClass | firstPseudoClass name < secondPseudoClass name].
	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: pseudoClasses)
		on: OrderedCollection new
		indent: ''.! !


!Browser methodsFor: 'class list' stamp: 'VGF 10/21/2024 02:01:50'!
createHierarchyTreeOf: classes
	"Create a tree from a collection of classes"

	^ self createHierarchyTreeOf: classes comparingWith: [:potentialSuperClass :potentialSubClass | potentialSuperClass = potentialSubClass superclass].! !


!CodeFileBrowser methodsFor: 'class list' stamp: 'VGF 10/21/2024 02:02:34'!
classList
	"Answer a collection of the class names of the selected category. Answer an 
	empty array if no selection exists."

	^(selectedSystemCategory isNil or: [ caseCodeSource isNil ])
		ifTrue: [ #() ]
		ifFalse: [self hierarchicalClassList]! !

