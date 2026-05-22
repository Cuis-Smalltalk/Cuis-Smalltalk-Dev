Class {
	#name : 'HierarchyBrowser',
	#superclass : 'Browser',
	#instVars : [
		'classList',
		'centralClass'
	],
	#category : 'Tools-Browser',
	#package : 'Tools',
	#tag : 'Browser'
}

{ #category : 'instance creation' }
HierarchyBrowser class >> for: aClass [
	"Open a new HierarchyBrowser on the given class"
	^HierarchyBrowser new initHierarchyForClass: aClass
]

{ #category : 'instance creation' }
HierarchyBrowser class >> for: aClass selector: aSymbol [
	"Open a new HierarchyBrowser on the given class"
	|  newBrowser |
	newBrowser := HierarchyBrowser new initHierarchyForClass: aClass.
	newBrowser setSelector: aSymbol.
	^newBrowser
]

{ #category : 'ui' }
HierarchyBrowser class >> open [
	^ HierarchyBrowserWindow onClass: Object selector: nil
]

{ #category : 'class list' }
HierarchyBrowser >> assureSelectionsShow [
	"This is a workaround for the fact that a hierarchy browser, when launched, often does not show the selected class.
	(jmv) Is this really needed?"

	| saveCatIndex saveMsgIndex |
	saveCatIndex := self messageCategoryListIndex.
	saveMsgIndex := self messageListIndex.
	self classListIndex: self classListIndex.
	self messageCategoryListIndex: saveCatIndex.
	self messageListIndex: saveMsgIndex
]

{ #category : 'class list' }
HierarchyBrowser >> classList [

	classList := classList select: [:each | Smalltalk includesKey: each withoutLeadingBlanks asSymbol].
	^ classList
]

{ #category : 'initialization' }
HierarchyBrowser >> classListIndex: newIndex [
	"Cause system organization to reflect appropriate category"

	| newClassName index |

	(classList isInBounds: newIndex) ifTrue: [
		newClassName := (classList at: newIndex) withoutLeadingBlanks.
		selectedSystemCategory := systemOrganizer categoryOfElement: newClassName].
	index := super classListIndex: newIndex.
	self changed: #systemCategorySingleton.
	^ index
]

{ #category : 'initialization' }
HierarchyBrowser >> defaultBrowserTitle [
	^ 'Hierarchy Browser'
]

{ #category : 'class functions' }
HierarchyBrowser >> defineClass: defString notifying: aRequestor   [
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass := self selectedClassOrMetaClass.
	defTokens := defString findTokens: Character separators.
	keywdIx := defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx := defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asPlainString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass := oldClass superclass ].
	class := Compiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryRoots.
				self updateAfterClassChange.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]
]

{ #category : 'initialization' }
HierarchyBrowser >> initAlphabeticListing [
	| tab stab index |
	self systemOrganizer: SystemOrganization.
	metaClassIndicated := false.
	classList := Smalltalk classNames.
]

{ #category : 'initialization' }
HierarchyBrowser >> initHierarchyForClass: aClassOrMetaClass [
	| tab stab index nonMetaClass |
	centralClass := aClassOrMetaClass.
	nonMetaClass := aClassOrMetaClass theNonMetaClass.
	self systemOrganizer: SystemOrganization.
	metaClassIndicated := aClassOrMetaClass isMeta.
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
	self classListIndex: index
]

{ #category : 'initialization' }
HierarchyBrowser >> potentialClassNames [
	
	"Answer the names of all the classes that could be viewed in this browser"
	
	^ self classList collect: [:aName | aName withoutLeadingBlanks ]
]

{ #category : 'menu messages' }
HierarchyBrowser >> removeSystemCategory [
	"If a class category is selected, create a Confirmer so the user can 
	verify that the currently selected class category and all of its classes
 	should be removed from the system. If so, remove it."

	self inform: 'Use a normal Browser, in which you can see 
the entire category you are trying to remove.'
]

{ #category : 'initialization' }
HierarchyBrowser >> systemCategorySingleton [

	| cls |
	cls := self selectedClass.
	^ cls ifNil: [#()]
		ifNotNil: [Array with: cls category]
]

{ #category : 'initialization' }
HierarchyBrowser >> updateAfterClassChange [
	"It is possible that some the classes comprising the hierarchy have changed, so reinitialize the entire browser."

	(centralClass notNil and: [centralClass isObsolete not])
		ifTrue: [self initHierarchyForClass: centralClass]
]
