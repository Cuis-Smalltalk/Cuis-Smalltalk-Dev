'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 3:37:20 pm'!

!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 13:49:41'!
classItems

	^ self classesToClassItems collect: #value! !

!Browser methodsFor: 'private' stamp: 'FJG 4/Apr/2026 13:47:02'!
classesToClassItems

	^ listClassesHierarchically
		ifTrue: [self hierarchicalClassList]
		ifFalse: [self basicClassList collect: [:aClass | aClass -> aClass name]].! !

!Browser methodsFor: 'dependency mechanism' stamp: 'FJG 4/Apr/2026 13:52:03'!
changed: aSymbol
	
	super changed: aSymbol.

	aSymbol == #classList ifTrue: [
		self changed: #classItems
	]! !


!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 13:53:45'!
classList

	^ self classesToClassItems collect: #key! !

!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 14:08:03'!
classListIndex: anInteger
	"Set anInteger to be the index of the current class selection."

	| className recent canSelectClass classList prevSelectedClassName prevSelectedMessage |
	classList := self classList.
	canSelectClass := classList isInBounds: anInteger.
	prevSelectedClassName := selectedClassName.
	selectedClassName := canSelectClass ifTrue: [
		classList at: anInteger
			ifPresent: [:newClass | newClass name asSymbol]
			ifAbsent: nil
	].
	self setClassOrganizer.
	prevSelectedMessage := selectedMessage.
	selectedMessage := nil.

	self classCommentIndicated ifFalse: [
		self editSelection: (canSelectClass
			ifTrue: [#editClass]
			ifFalse: [ metaClassIndicated | selectedSystemCategory isNil
				ifTrue: [#none]
				ifFalse: [#newClass]])].

	self selectedClass ifNotNil: [
		recent := self class recentClasses.
		className := self selectedClass name.
		(recent includes: className) ifTrue: [recent remove: className].
		recent addFirst: className.
		recent size > 16 ifTrue: [recent removeLast]].

	"Clear selectedMessageCategory if there is no match in the new list of categories"
	(self messageCategoryList indexOf: selectedMessageCategory) = 0 ifTrue: [
		selectedMessageCategory := nil].

	self changed: #classCommentText.
	self changed: #classListIndex.	"update my selection"
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged.
	prevSelectedClassName = selectedClassName ifFalse: [
		"If clicked on already selected class, deselect selector, so class definition is shown"
		self setSelector: prevSelectedMessage ].! !

!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 14:03:25'!
classListIndexOf: classNameToFind

	"Answer the index of the classNameToFind selection."

	classNameToFind ifNil: [ ^0 ].
	^ self classList findFirst: [:aClass | aClass name = classNameToFind]! !

!Browser methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 13:52:59'!
flattenHierarchyTree: classHierarchy on: col indent: indent by: indentChars

	"Recursively add to col the names in classHierarchy indenting to show the hierarchical relationship. Use indentChars to do the indenting: spaces, tabs, etc."

	| plusIndent |

	plusIndent := indentChars.
	classHierarchy do: [:assoc |
		| class childs |
		class := assoc key.
		col add: class -> (indent , class name).
		childs := assoc value.
		self
			flattenHierarchyTree: childs
			on: col
			indent: indent , plusIndent
			by: indentChars].
	^ col! !


!HierarchyBrowser methodsFor: 'initialization' stamp: 'FJG 4/Apr/2026 14:57:32'!
classListIndex: newIndex
	"Cause system organization to reflect appropriate category"

	| newClassName classes |
	classes := self classList.
	(classes isInBounds: newIndex) ifTrue: [
		newClassName := (classes at: newIndex) name.
		selectedSystemCategory := systemOrganizer categoryOfElement: newClassName].
	super classListIndex: newIndex.
	self changed: #systemCategorySingleton.! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'FJG 4/Apr/2026 14:01:34'!
buildMorphicClassList

	^(PluggableListMorph
		withModel: model
		listGetter: #classItems
		indexGetter: #classListIndex
		indexSetter: #classListIndex:)
			menuNshortcutsKey: #classListMenuSpec;
			doubleClickSelector: #browseMethodFull.! !


!CodeFileBrowserWindow methodsFor: 'menu commands' stamp: 'FJG 4/Apr/2026 15:26:55'!
findClass
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	self request: 'Class name?' do: [:aString|
		aString isEmpty ifFalse:
			[pattern := aString asLowercase.
			classNames := Set new.
			classNames addAll: model caseCodeSource classDictionary keys.
			classNames := classNames asArray select: 
				[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
			classNames isEmpty ifFalse:
				[index := classNames size = 1
					ifTrue: [1]
					ifFalse: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu].
				index = 0 ifFalse:
					[foundCodeFile := nil.
					foundClass := nil.
					(model caseCodeSource classDictionary includesKey: (classNames at: index))
						ifTrue:
							[foundClass := model caseCodeSource classDictionary at: (classNames at: index).
							foundCodeFile := model caseCodeSource].
					foundClass ifNotNil:
						[model setSelectedSystemCategory: foundCodeFile name asSymbol.
						model selectClass: foundClass]]]]]! !

'Migrate existing instances of BrowserWindow'!
BrowserWindow allSubInstances do: [:browserWindow |
	browserWindow allSubmorphsDo: [:aMorph |
		aMorph isMemberOf: PluggableListMorph
			:: and: [ (aMorph instVarNamed: 'getListSelector') = #classList ]
			:: ifTrue: [
				aMorph instVarNamed: 'getListSelector' put: #classItems
			]
	]
]!

