'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 4:23:03 pm'!

!CodeProvider methodsFor: 'private' stamp: 'FJG 4/Apr/2026 15:50:06'!
labelForClass: aClassOrPseudoClass
	"This sets up a single extension point: we can override labelForClass: to customize how
class names appear on the class lists without touching the list-building logic."
	^ aClassOrPseudoClass name! !


!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 15:50:53'!
flattenHierarchyTree: classHierarchy on: col indent: indent by: indentChars

	"Recursively add to col the names in classHierarchy indenting to show the hierarchical relationship. Use indentChars to do the indenting: spaces, tabs, etc."

	| plusIndent |

	plusIndent := indentChars.
	classHierarchy do: [:assoc |
		| class childs |
		class := assoc key.
		col add: class -> (indent , (self labelForClass: class)).
		childs := assoc value.
		self
			flattenHierarchyTree: childs
			on: col
			indent: indent , plusIndent
			by: indentChars].
	^ col! !

!Browser methodsFor: 'private' stamp: 'FJG 4/Apr/2026 15:50:30'!
classesToClassItems

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self basicClassList collect: [:aClass | aClass -> self labelForClass: aClass ]].! !


!ChangeSorter methodsFor: 'private' stamp: 'FJG 4/Apr/2026 15:53:32'!
classesToClassItems
	"Computed.  View should try to preserve selections, even though index changes.
	A class may have been removed from the system but still be in the change set.
	Use a PseudoClass as a stand-in so the list always contains class-like objects."

	^ (myChangeSet ifNotNil: [myChangeSet changedClassNames] ifNil: [OrderedCollection new])
		collect: [:className |
			|theClass|
			theClass := (Smalltalk classNamed: className)
				ifNil: [PseudoClass new name: className].
			theClass -> (self labelForClass: theClass)
		]
! !

