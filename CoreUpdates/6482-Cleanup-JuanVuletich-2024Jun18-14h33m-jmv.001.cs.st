'From Cuis7.1 [latest update: #6487] on 18 June 2024 at 2:35:25 pm'!

!Browser methodsFor: 'class list' stamp: 'jmv 6/18/2024 11:13:51'!
basicClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	selectedSystemCategory ifNil: [^ #()].
	^ systemOrganizer fullListAtCategoryNamed: selectedSystemCategory! !


!SinglePackageBrowser methodsFor: 'lists' stamp: 'jmv 6/18/2024 11:13:49'!
basicClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."
	| answer |
	answer := selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [
			(package includesSystemCategory: selectedSystemCategory)
				ifTrue: [ systemOrganizer listAtCategoryNamed: selectedSystemCategory ]
				ifFalse: [ 
					package extensionClassNamesIn: (selectedSystemCategory copyFrom: 2 to: selectedSystemCategory size) ]].
	selectedClassName ifNil: [
		answer isEmpty ifFalse: [
			selectedClassName := answer first.
			self setClassOrganizer.
			self editSelection: #editClass ]].
	^answer! !


!Browser methodsFor: 'class list' stamp: 'jmv 6/18/2024 11:14:06'!
classList

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self basicClassList].! !

!Browser methodsFor: 'class list' stamp: 'jmv 6/18/2024 11:14:45'!
hierarchicalClassList
	"classNames are an arbitrary collection of classNames of the system.
	Reorder those class names so that they are sorted and indended by inheritance"

	| classes |
	classes := self basicClassList collect: [:sym | Smalltalk classNamed: sym].
	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: classes)
		on: OrderedCollection new
		indent: ''.
! !

!methodRemoval: SinglePackageBrowser #defaultClassList stamp: 'jmv 6/18/2024 14:35:03'!
SinglePackageBrowser removeSelector: #defaultClassList!
!methodRemoval: Browser #defaultClassList stamp: 'jmv 6/18/2024 14:35:03'!
Browser removeSelector: #defaultClassList!
