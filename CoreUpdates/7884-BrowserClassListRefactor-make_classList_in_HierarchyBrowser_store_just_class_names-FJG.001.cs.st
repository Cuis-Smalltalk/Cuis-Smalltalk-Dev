'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 1:17:41 pm'!

!HierarchyBrowser methodsFor: 'initialization' stamp: 'FJG 4/Apr/2026 13:12:58'!
classListIndex: newIndex
	"Cause system organization to reflect appropriate category"

	| newClassName |
	(classList isInBounds: newIndex) ifTrue: [
		newClassName := (classList at: newIndex).
		selectedSystemCategory := systemOrganizer categoryOfElement: newClassName].
	super classListIndex: newIndex.
	self changed: #systemCategorySingleton.! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'FJG 4/Apr/2026 13:15:16'!
initHierarchyForClass: aClassOrMetaClass
	| nonMetaClass superclasses subclasses |
	centralClass := aClassOrMetaClass.
	nonMetaClass := aClassOrMetaClass theNonMetaClass.
	self systemOrganizer: SystemOrganization.
	metaClassIndicated := aClassOrMetaClass isMeta.
	listClassesHierarchically := true.
	
	superclasses := nonMetaClass allSuperclasses.
	subclasses := nonMetaClass allSubclasses.
	classList := superclasses, {nonMetaClass}, subclasses
		collect: [:aClass | aClass name].
	
	self classListIndex: superclasses size + 1! !

!HierarchyBrowser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 13:14:52'!
basicClassList
	^ (classList sorted select: [:name | Smalltalk hasClassNamed: name ])
		collect: [:name | Smalltalk classNamed: name]! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'FJG 4/Apr/2026 13:16:25'!
potentialClassNames
	
	"Answer the names of all the classes that could be viewed in this browser"
	
	^ self basicClassList collect: [:aClass | aClass name ]! !

!PseudoClass methodsFor: 'accessing' stamp: 'FJG 4/Apr/2026 13:16:03'!
allSubclasses
	^ (self realClass ifNil: [ ^#() ]) allSubclasses! !

'Migrate existing instances of HierarchyBrowser'!
HierarchyBrowser allSubInstances do: [:hierarchyBrowser |
	hierarchyBrowser instVarNamed: 'classList' put: (
		(hierarchyBrowser instVarNamed: 'classList')
			collect: [:className | className withoutLeadingBlanks ]
	)
]!
