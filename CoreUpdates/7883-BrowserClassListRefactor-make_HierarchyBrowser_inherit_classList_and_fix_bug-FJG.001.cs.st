'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 1:04:05 pm'!

!HierarchyBrowser methodsFor: 'initialization' stamp: 'FJG 4/Apr/2026 12:58:53'!
initAlphabeticListing
	
	self systemOrganizer: SystemOrganization.
	metaClassIndicated := false.
	listClassesHierarchically := false.
	classList := Smalltalk classNames.! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'FJG 4/Apr/2026 12:59:24'!
initHierarchyForClass: aClassOrMetaClass
	| tab stab index nonMetaClass |
	centralClass := aClassOrMetaClass.
	nonMetaClass := aClassOrMetaClass theNonMetaClass.
	self systemOrganizer: SystemOrganization.
	metaClassIndicated := aClassOrMetaClass isMeta.
	listClassesHierarchically := true.
	classList := OrderedCollection new.
	tab := ''.
	nonMetaClass allSuperclasses reverseDo: 
		[:aClass | 
		classList add: tab , aClass name.
		tab := tab , '  '].
	index := classList size + 1.
	nonMetaClass allSubclassesWithLevelDo:
		[:aClass :level |
		stab := ''.  1 to: level do: [:i | stab := stab , '  '].
		classList add: tab , stab , aClass name]
	 	startingLevel: 0.
	self classListIndex: index! !

!HierarchyBrowser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 13:01:56'!
basicClassList
	^ ((classList select: [:name | Smalltalk includesKey: name withoutLeadingBlanks asSymbol])
		collect: [:name | Smalltalk at: name withoutLeadingBlanks asSymbol])
		sorted: [:a :b | a name <= b name]! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'FJG 4/Apr/2026 13:03:15'!
classListIndex: newIndex
	"Cause system organization to reflect appropriate category"

	| newClassName classNames |
	classNames := self classList.
	(classNames isInBounds: newIndex) ifTrue: [
		newClassName := (classNames at: newIndex) withoutLeadingBlanks.
		selectedSystemCategory := systemOrganizer categoryOfElement: newClassName].
	super classListIndex: newIndex.
	self changed: #systemCategorySingleton.! !

!methodRemoval: HierarchyBrowser #classList stamp: 'FJG 4/Apr/2026 12:59:53'!
HierarchyBrowser removeSelector: #classList!
