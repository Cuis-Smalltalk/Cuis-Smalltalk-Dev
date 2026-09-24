'From Cuis7.9 [latest update: #8210] on 22 September 2026 at 6:21:28 pm'!

!CodeProvider methodsFor: 'message list' stamp: 'FJG 6/Apr/2026, 3:31:41 pm (UTC)'!
messageItems

	^ self messageList collect: [:aSelector | self labelForSelector: aSelector]! !

!CodeProvider methodsFor: 'private' stamp: 'FJG 6/Apr/2026, 4:18:33 pm (UTC)'!
labelForSelector: aSelector
	"This sets up a single extension point: we can override labelForSelector: to customize how
selectors appear on the message lists without touching the list-building logic."
	^ aSelector! !

!CodeProvider methodsFor: 'dependency mechanism' stamp: 'FJG 6/Apr/2026, 3:31:22 pm (UTC)'!
changed: aSymbol

	super changed: aSymbol.

	aSymbol = #messageList ifTrue: [
		self changed: #messageItems
	]! !


!CodeFileBrowser methodsFor: 'private' stamp: 'FJG 6/Apr/2026, 4:06:35 pm (UTC)'!
labelForSelector: aSelector
	| textAttribute originalLabel |
	textAttribute := (self classOrMetaClassOrganizer isRemoved: aSelector)
		ifTrue: [ TextColor red ]
		ifFalse: [ | baseSource |
			baseSource := self pvtBaseSelectedMessageSourceCodeFor: aSelector.
			baseSource
				ifNil: [ TextColor green ]
				ifNotNil: [ | caseSource |
					caseSource := self pvtCaseSelectedMessageSourceCodeFor: aSelector.
					baseSource = caseSource ifFalse: [ TextColor blue ]]].
			
	originalLabel := super labelForSelector: aSelector.
	^ textAttribute
		ifNil: [ originalLabel ]
		ifNotNil: [ :attribute | originalLabel asText addAttribute: attribute ]! !


!Browser methodsFor: 'message category functions' stamp: 'FJG 8/Apr/2026, 3:32:38 pm (UTC)'!
categorizeUnderCategoryAt: aMessageCategoryListIndex selector: aMessageItemToCategorize
	| aSelectorToCategorize selectorIndex |
	
	selectorIndex := self messageItems findFirst: [:item | item = aMessageItemToCategorize].
	selectorIndex <= 0 ifTrue: [ ^ self ].
	aSelectorToCategorize := self messageList at: selectorIndex.

	aMessageCategoryListIndex isNil ifTrue: [^ self categorizeUnderNewCategorySelector: aSelectorToCategorize].
	self selectedClassOrMetaClass ifNotNil: [ :class | | categorySelector |
		categorySelector := self messageCategoryList at: aMessageCategoryListIndex ifAbsent: [^self].
		categorySelector ~= Categorizer allCategory
			ifTrue: [
				class organization classify: aSelectorToCategorize under: categorySelector suppressIfDefault: false.
				self changed: #messageList]]! !

!Browser methodsFor: 'message list' stamp: 'FJG 6/Apr/2026, 3:33:17 pm (UTC)'!
messageListIndex: anInteger
	"Set the selected message selector to be the one indexed by anInteger."
	
	| index messageList |
	
	messageList := self messageList.
	index := messageList ifInBounds: anInteger ifNot: 0.

	selectedMessage := index = 0 ifFalse: [ messageList at: index ].
	self editSelection: (index > 0
		ifTrue: [#editMessage]
		ifFalse: [self messageCategoryListIndex > 0
			ifTrue: [#newMessage]
			ifFalse: [self classListIndex > 0
				ifTrue: [#editClass]
				ifFalse: [#newClass]]]).
	self changed: #messageListIndex. "update my selection"
	self changed: #relabel.
	self acceptedContentsChanged! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'FJG 6/Apr/2026, 3:34:34 pm (UTC)'!
buildMorphicMessageList

	^(PluggableListMorph
		withModel: model
		listGetter: #messageItems
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec;
			doubleClickSelector: #browseMethodFull.! !

!methodRemoval: CodeFileBrowser #messageList stamp: 'FJG 22/Sep/2026, 9:20:24 pm (UTC)'!
CodeFileBrowser removeSelector: #messageList!

'Migrate existing instances of BrowserWindow'!
BrowserWindow allSubInstances do: [:browserWindow |
	browserWindow allSubmorphsDo: [:aMorph |
		aMorph isMemberOf: PluggableListMorph
			:: and: [ (aMorph instVarNamed: 'getListSelector') = #messageList ]
			:: ifTrue: [
				aMorph instVarNamed: 'getListSelector' put: #messageItems
			]
	]
]!

