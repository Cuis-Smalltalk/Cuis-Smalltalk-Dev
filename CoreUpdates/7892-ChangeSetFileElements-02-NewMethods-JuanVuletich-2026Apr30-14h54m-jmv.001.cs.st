'From Cuis7.7 [latest update: #7890] on 30 April 2026 at 2:55:26 pm'!

!ChangeList commentStamp: '<historical>' prior: 0!
A ChangeList represents a list of changed methods that reside on a file in fileOut format.  The classes and methods in my list are not necessarily in this image!!  Used as the model for both Version Lists and Changed Methods.

It holds three lists:
	changeList - a list of ChangeListElements
	list - a list of one-line printable headers
	listSelections - a list of Booleans (true = selected, false = not selected) multiple OK.
	listIndex 
Items that are removed (removeDoits, remove an item) are removed from all three lists.
Most recently clicked item is the one showing in the bottom pane.!

!classDefinition: #VersionsBrowser category: #'Tools-Changes'!
ChangeList subclass: #VersionsBrowser
	instanceVariableNames: 'classOfMethod selectorOfMethod addedChangeRecord addedChange '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 30/Apr/2026 11:36:02'!
recordedChangesForSelector: selector
	"Return a list of ChangeListElementss for all versions of the method at selector. Source code can be retrieved by sending string to any one.  Return nil if the method is absent."

	| aList |
	aList := VersionsBrowser new
			scanVersionsOf: (self compiledMethodAt: selector ifAbsent: [^ nil])
			class: self meta: self isMeta
			category: (self whichCategoryIncludesSelector: selector)
			selector: selector.
	^ aList ifNotNil: [aList changeList]! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 30/Apr/2026 11:49:16'!
scanSpecificChangeType
	"Scan anything that involves more than one chunk"

	| itemPosition item tokens firstToken secondToken |

	itemPosition := file position.
	item := file nextChunk.

	(self itemIsRecognized: item) ifFalse: [
		"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
		^ self scanAndIgnore: item in: itemPosition ].

	tokens := Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		firstToken := tokens first.
		secondToken := tokens second.

		firstToken == #classDefinition:
			ifTrue: [ ^ self scanClassDefinition: tokens ].
		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ])
			ifTrue: [ ^ self scanClassRemoval: tokens ].
		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ])
			ifTrue: [ ^ self scanMethodRemoval: tokens ].
		(secondToken == #methodsFor: or: [ 
			tokens size > 2 and: [ tokens third == #methodsFor: ] ])
			ifTrue: [ ^ self scanMethodDefinition: tokens ].
		secondToken == #commentStamp:
			ifTrue: [ ^ self scanClassComment: tokens ].
		firstToken == #provides:
			ifTrue: [ ^ self scanFeatureProvision: tokens ].
		firstToken == #requires:
			ifTrue: [ ^ self scanFeatureRequirement: tokens ].
		firstToken == #classRenamed:
			ifTrue: [ ^ self scanClassRenamed: tokens ].
		]! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 30/Apr/2026 11:52:15'!
extraChange: aChange
	addedChange := aChange.
	self reformulateList.! !


!ClassCommentVersionsBrowser class methodsFor: 'utilities' stamp: 'jmv 30/Apr/2026 11:47:19'!
commentsOf: aClass
	"Return a list of ClassCommentChanges for all versions of the method at selector. Source code can be retrieved by sending string to any one.  Return nil if the method is absent."

	| aList |
	aList := self new
			scanVersionsOf: aClass.
	^ aList ifNotNil: [aList changeList]! !


!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 30/Apr/2026 11:47:43'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector extraChange: aChange
	^ self browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: nil extraChange: aChange! !

!VersionsBrowserWindow class methodsFor: 'services' stamp: 'jmv 30/Apr/2026 11:50:21'!
browseVersionsOf: method class: class meta: meta category: msgCategory selector: selector lostMethodPointer: sourcePointer extraChange: aChange
	| browser |
	browser := VersionsBrowser new.
	(browser scanVersionsOf: method class: class meta: meta category: msgCategory selector: selector)
		ifNil: [ self inform: 'No versions available'. ^nil ].
	aChange ifNotNil: [browser extraChange: aChange ].
	browser toggleListIndex: 1.
	VersionsBrowserWindow open: browser label: 'Recent versions of ', selector.
	^browser! !


!ChangeListElement methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:47:37'!
isClassComment
	^false! !

!ChangeListElement methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:35:54'!
isClassDefinition
	^false! !

!ChangeListElement methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:40:09'!
isMethod

	^false! !

!ChangeListElement methodsFor: 'testing' stamp: 'jmv 30/Apr/2026 10:35:03'!
isMethodDefinition

	^false! !

!ChangeListElement methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 12:28:49'!
changeClassName
	^nil! !

!ChangeListElement methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026 12:31:08'!
compilerClass
	^ Compiler! !

!methodRemoval: ClassDescription #changeRecordsAt: stamp: 'jmv 30/Apr/2026 15:07:09'!
ClassDescription removeSelector: #changeRecordsAt:!
!classDefinition: #VersionsBrowser category: #'Tools-Changes'!
ChangeList subclass: #VersionsBrowser
	instanceVariableNames: 'classOfMethod selectorOfMethod addedChangeRecord addedChange'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
