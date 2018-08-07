'From Cuis 5.0 of 7 November 2016 [latest update: #3389] on 2 August 2018 at 8:36:25 pm'!

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 20:19:45'!
classListIndexOf: aClassNameToFind

	"Answer the index of the aClassName selection."

	aClassNameToFind ifNil: [ ^0 ].
	^self listClassesHierarchically 
		ifTrue: [ self classListIndexWhenShowingHierarchicallyOf: aClassNameToFind ]
		ifFalse: [ self classList indexOf: aClassNameToFind ]
! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 20:20:58'!
classListIndexWhenShowingHierarchicallyOf: aClassNameToFind

	^self classList findFirst: [ :showingClassName | showingClassName afterBlanksEndsWith: aClassNameToFind  ]
! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:37:45'!
createHierarchyTreeOf: col

	"Create a tree from a flat collection of classes"

	| transformed |

	transformed := col collect: [:ea | 
		| childs indexes |
		childs := col select: [:class | class superclass = ea].
		indexes := childs collect: [:child | col indexOf: child].
		ea -> indexes].
	transformed copy do: [:ea |
		ea value: (ea value collect: [:idx | 
			| val |
			val := transformed at: idx.
			transformed at: idx put: nil.
			val])].
	^ transformed select: [:ea | ea notNil].
! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 17:51:43'!
defaultClassList
	"Answer an array of the class names of the selected category. Answer an 
	empty array if no selection exists."

	^selectedSystemCategory
		ifNil: [#()]
		ifNotNil: [systemOrganizer listAtCategoryNamed: selectedSystemCategory]! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:38:01'!
flattenHierarchyTree: classHierarchy on: col indent: indent

	^ self
		flattenHierarchyTree: classHierarchy
		on: col
		indent: indent
		by: '  '.! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:38:11'!
flattenHierarchyTree: classHierarchy on: col indent: indent by: indentChars

	"Recursively add to col the names in classHierarchy indenting to show the hierarchical relationship. Use indentChars to do the indenting: spaces, tabs, etc."

	| plusIndent |

	plusIndent := indentChars.
	classHierarchy do: [:assoc |
		| class childs |
		class := assoc key.
		col add: indent , class name.
		childs := assoc value.
		self
			flattenHierarchyTree: childs
			on: col
			indent: indent , plusIndent
			by: indentChars].
	^ col! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:38:37'!
hierarchicalClassList

	"classNames are an arbitrary collection of classNames of the system.
	Reorder those class names so that they are sorted and indended by inheritance"

	| classes |

	"Creating the hierarchy is *really slow* for the full class list. Skip it for now."
	selectedSystemCategory = SystemOrganizer allCategory ifTrue: [^ self defaultClassList].		
	classes := self defaultClassList collect: [:sym | Smalltalk classNamed: sym].
	
	^ self
		flattenHierarchyTree: (self createHierarchyTreeOf: classes)
		on: OrderedCollection new
		indent: ''.! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:42:13'!
listClassesHierarchically 

	^self class listClassesHierarchically ! !


!Browser class methodsFor: 'class list' stamp: 'HAW 8/2/2018 20:25:05'!
disableListClassesHierarchically
	
	^Preferences disable: #listClassesHierarchically ! !

!Browser class methodsFor: 'class list' stamp: 'HAW 8/2/2018 20:24:55'!
enableListClassesHierarchically
	
	^Preferences enable: #listClassesHierarchically ! !

!Browser class methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:21:34'!
listClassesHierarchically
	
	^Preferences listClassesHierarchically ! !


!String methodsFor: 'converting' stamp: 'HAW 8/2/2018 20:19:09'!
afterBlanksEndsWith: aTail

	^(self endsWith: aTail) and: [self size = aTail size or: [ (self at: self size - aTail size) isSeparator]]
					
	! !

!String methodsFor: 'converting' stamp: 'HAW 8/2/2018 19:11:17'!
withoutLeadingBlanks
	
	"Return a copy of the receiver from which leading blanks have been trimmed."
	
	| first |
	
	first := self findFirst: [:c | c isSeparator not ].
	first = 0 ifTrue: [^ ''].  
	first = 1 ifTrue: [^ self ].
	
	"no non-separator character"	
	^ self copyFrom: first to: self size
		
	" '    abc  d  ' withoutLeadingBlanks"
! !


!Preferences class methodsFor: 'standard queries'!
listClassesHierarchically
	^ self
		valueOfFlag: #listClassesHierarchically
		ifAbsent: [ true ].! !


!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:42:28'!
classList

	^ self listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self defaultClassList].! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:42:49'!
classListIndex
	"Answer the index of the current class selection."

	^self classListIndexOf: selectedClassName ! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:35:07'!
classListIndex: anInteger 
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList |

	classList _ self classList.
	canSelectClass _ classList isInBounds: anInteger.
	selectedClassName _ canSelectClass ifTrue: [	| newClassName |
		newClassName := classList at: anInteger ifAbsent: [ nil ].
		newClassName := newClassName ifNotNil: [newClassName withoutLeadingBlanks asSymbol].
		newClassName ].
	self setClassOrganizer.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	
	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent _ self class recentClasses.
		className _ self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].
	
	"We have to refresh the class list if somebody wants to select a class that does not exist anymore - Hernan"
	anInteger > classList size ifTrue: [ self changed: #classList ].
	self changed: #classSelectionChanged.
	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:44:06'!
selectClass: classNotMeta

	self classListIndex: (self classListIndexOf: classNotMeta name)! !

!Browser methodsFor: 'class list' stamp: 'HAW 8/2/2018 19:45:38'!
selectedClassName
	"Answer the name of the current class. Answer nil if no selection exists."

	^selectedClassName
	! !

!Browser methodsFor: 'metaclass' stamp: 'HAW 8/2/2018 19:45:57'!
setClassOrganizer

	"Install whatever organization is appropriate"

	| theClass |

	classOrganizer _ nil.
	metaClassOrganizer _ nil.
	selectedClassName ifNil: [^ self].	
	theClass := self selectedClass ifNil: [ ^self ].
	classOrganizer _ theClass organization.
	metaClassOrganizer _ theClass class organization.! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'HAW 8/2/2018 20:06:54'!
classListIndex: newIndex

	"Cause system organization to reflect appropriate category"

	| newClassName ind i |

	(classList isInBounds: newIndex) ifTrue: [
		newClassName _ (classList at: newIndex) withoutLeadingBlanks.
		i _ systemOrganizer numberOfCategoryOfElement: newClassName.
		selectedSystemCategory _ i = 0 ifFalse: [ self systemCategoryList at: i]].
	ind _ super classListIndex: newIndex.
	self changed: #systemCategorySingleton.
	^ ind! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'HAW 8/2/2018 20:07:42'!
potentialClassNames
	
	"Answer the names of all the classes that could be viewed in this browser"
	
	^ self classList collect: [:aName | aName withoutLeadingBlanks ]! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'HAW 8/2/2018 20:21:08'!
selectClass: classNotMeta
	
	self classListIndex: (self classListIndexWhenShowingHierarchicallyOf: classNotMeta name)! !

!HierarchyBrowser methodsFor: 'initialization' stamp: 'HAW 8/2/2018 20:10:24'!
selectedClassName
	"Answer the name of the class currently selected.   di
	  bug fix for the case where name cannot be found -- return nil rather than halt"

	| aName |
	
	aName _ super selectedClassName.
	^ aName ifNotNil: [aName withoutLeadingBlanks asSymbol]! !

!HierarchyBrowser methodsFor: 'class list' stamp: 'HAW 8/2/2018 20:06:27'!
classList

	classList _ classList select: [:each | Smalltalk includesKey: each withoutLeadingBlanks asSymbol].
	^ classList! !


!SequenceableCollection methodsFor: 'enumerating' stamp: 'HAW 8/2/2018 19:17:40'!
findFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true."

	| index currentSize |
	
	index _ 0.
	currentSize _ self size.
	
	[(index _ index + 1) <= currentSize ] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	
	^ 0! !


!String methodsFor: 'converting' stamp: 'HAW 8/2/2018 19:17:01'!
withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	
	last _ self findLast: [:c | c isSeparator not].
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	last = self size ifTrue: [ ^self ].
	
	^ self copyFrom: 1 to: last

	" ' abc  d   ' withoutTrailingBlanks"
! !


!Browser class reorganize!
('instance creation' new)
('cached state access' recentClasses releaseClassCachedState)
('class list' disableListClassesHierarchically enableListClassesHierarchically listClassesHierarchically)
!

