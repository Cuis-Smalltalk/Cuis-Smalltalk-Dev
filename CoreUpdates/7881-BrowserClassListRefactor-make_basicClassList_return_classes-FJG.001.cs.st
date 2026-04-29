'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 12:44:39 pm'!

!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 12:43:09'!
basicClassList
	"Answer an array of the classes of the selected category. Answer an 
	empty array if no selection exists."
	selectedSystemCategory ifNil: [^ #()].
	^ (systemOrganizer fullListAtCategoryNamed: selectedSystemCategory)
		 collect: [:className | Smalltalk classNamed: className]! !

!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 12:43:18'!
classList

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self basicClassList collect: [:aClass | aClass name]].! !

!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 12:43:26'!
hierarchicalClassList
	"Reorder classes so that they are sorted and indented by inheritance"

	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: self basicClassList)
		on: OrderedCollection new
		indent: ''.
! !


!SinglePackageBrowser methodsFor: 'lists' stamp: 'FJG 4/Apr/2026 12:42:35'!
basicClassList
	"Answer an array of the classes of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer := selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer fullListAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn:
						(selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer isEmpty ifFalse: [
			selectedClassName := answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer collect: [:className | Smalltalk classNamed: className]! !

