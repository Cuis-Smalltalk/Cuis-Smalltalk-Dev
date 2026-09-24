'From Cuis7.9 [latest update: #8210] on 22 September 2026 at 6:25:07 pm'!

!ChangeSorter methodsFor: 'message list' stamp: 'FJG 6/Apr/2026, 4:31:21 pm (UTC)'!
messageListIndex

	currentSelector ifNil: [ ^0 ].
	^ self messageList findFirst: [:aSelector | aSelector = currentSelector]! !

!ChangeSorter methodsFor: 'message list' stamp: 'FJG 6/Apr/2026, 4:33:22 pm (UTC)'!
messageListIndex: anInteger

	self currentSelector: (self messageList at: anInteger ifAbsent: nil)! !


!ChangeSorter methodsFor: 'message list' stamp: 'FJG 22/Sep/2026, 1:01:54 pm (UTC)'!
currentSelector: messageName

	currentSelector := messageName.
	self changed: #currentSelector.
	self changed: #messageListIndex.
	self acceptedContentsChanged.! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'FJG 22/Sep/2026, 1:01:51 pm (UTC)'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| changeSetList classList changeSetListPane classListPane
	  messageList upperPanes labelBackground |
	labelBackground := Color gray alpha: 0.1.
	model myChangeSet ifNil: [
		"Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	changeSetList := (PluggableListMorphByItem
				withModel: model
				listGetter: #changeSetList
				indexGetter: #currentChangeSetName
				indexSetter: #showChangeSetNamed:)
					menuNshortcutsKey: #changeSetListMenuSpec;
					autoDeselect: false.
	changeSetListPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Change Set name');
		addMorphUseAll: changeSetList.

	classList := (PluggableListMorph
				withModel: model
				listGetter: #classItems
				indexGetter: #classListIndex
				indexSetter: #classListIndex:)
					menuNshortcutsKey: #classListMenuSpec.
	classListPane := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Classes');
		addMorphUseAll: classList.

	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: changeSetListPane proportionalWidth: 0.6;
		addAdjusterAndMorph: classListPane proportionalWidth: 0.4.
	
	messageList := (PluggableListMorph
				withModel: model
				listGetter: #messageItems
				indexGetter: #messageListIndex
				indexSetter: #messageListIndex:)
					menuNshortcutsKey: #messageListMenuSpec.
	messageList := LinearLayoutMorph newColumn
		color: labelBackground;
		addMorph: (ColoredBoxMorph new color: `Color transparent`) fixedHeight: 4;
		addMorphKeepMorphHeight: (LabelMorph new contents: 'Methods');
		addMorphUseAll: messageList.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: messageList proportionalHeight: 0.2;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.55.

	self setLabel: model labelString! !


'Migrate existing instances of ChangeSorterWindow'!
ChangeSorterWindow allSubInstances do: [:changeSorterWindow |
	changeSorterWindow allSubmorphsDo: [:aMorph |
		aMorph isMemberOf: PluggableListMorphByItem
			:: and: [ (aMorph instVarNamed: 'getListSelector') = #messageList ]
			:: ifTrue: [
				| newList |
				newList := (PluggableListMorph
					withModel: aMorph model
					listGetter: #messageItems
					indexGetter: #messageListIndex
					indexSetter: #messageListIndex:)
					menuNshortcutsKey: #messageListMenuSpec;
					adoptWidgetsColor: changeSorterWindow windowColor;
					layoutSpec: aMorph layoutSpec;
					yourself.
				aMorph owner replaceSubmorph: aMorph by: newList.
			].
	]
]!
Smalltalk garbageCollect!

