'From Cuis7.1 [latest update: #6480] on 18 June 2024 at 10:11:06 am'!

!Browser methodsFor: 'class list' stamp: 'jmv 6/18/2024 10:08:47'!
createHierarchyTreeOf: classes
	"Create a tree from a collection of classes. Keep ordering while doing it.
	The Each node in the tree is an Association with a class as key and a collection
	of child nodes as value."

	| classesWithSubclassIndexes |

	"For each class, collect the indexes of its included subclasses."
	classesWithSubclassIndexes := classes collect: [ :eachClass | 
		| subclasses subclassIndexes |
		subclasses := classes select: [ :class | class superclass = eachClass ].
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

