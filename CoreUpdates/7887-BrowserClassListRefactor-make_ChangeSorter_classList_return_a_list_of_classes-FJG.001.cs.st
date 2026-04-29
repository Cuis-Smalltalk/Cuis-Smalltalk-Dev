'From Cuis7.7 [latest update: #7850] on 4 April 2026 at 3:45:44 pm'!

!ChangeSorter methodsFor: 'private' stamp: 'FJG 4/Apr/2026 15:41:45'!
classesToClassItems
	"Computed.  View should try to preserve selections, even though index changes.
	A class may have been removed from the system but still be in the change set.
	Use a PseudoClass as a stand-in so the list always contains class-like objects."

	^ (myChangeSet ifNotNil: [myChangeSet changedClassNames] ifNil: [OrderedCollection new])
		collect: [:className |
			((Smalltalk classNamed: className)
				ifNil: [PseudoClass new name: className]) -> className ]
! !

!ChangeSorter methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 15:42:43'!
classItems

	^ self classesToClassItems collect: #value! !

!ChangeSorter methodsFor: 'dependency mechanism' stamp: 'FJG 4/Apr/2026 15:43:27'!
changed: aSymbol
	
	super changed: aSymbol.

	aSymbol == #classList ifTrue: [
		self changed: #classItems
	]! !


!ChangeSorter methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 15:42:24'!
classList

	^ self classesToClassItems collect: #key! !


!ChangeSorter methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 16:15:51'!
classListIndex
	
	currentClassName ifNil: [ ^0 ].
	^ self classList findFirst: [:aClass | aClass name = currentClassName]! !


!ChangeSorter methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 16:19:57'!
classListIndex: anInteger
	
	self currentClassName: (
		self classList at: anInteger
			ifPresent: [:aClass | aClass name]
			ifAbsent: nil
	)! !


!ChangeSorter methodsFor: 'class list' stamp: 'FJG 4/Apr/2026 16:18:43'!
currentClassName: aString

	currentClassName := aString.
	currentSelector := nil.	"fix by wod"
	self changed: #classListIndex.
	self changed: #messageList.
	self acceptedContentsChanged.! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'FJG 4/Apr/2026 16:18:30'!
buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| changeSetList classList changeSetListPane classListPane
	  messageList upperPanes labelBackground |
	labelBackground := Theme current background.
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
	
	messageList := (PluggableListMorphByItem
				withModel: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:)
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
			:: and: [ (aMorph instVarNamed: 'getListSelector') = #classList ]
			:: ifTrue: [
				| newList |
				newList := (PluggableListMorph
					withModel: aMorph model
					listGetter: #classItems
					indexGetter: #classListIndex
					indexSetter: #classListIndex:)
					menuNshortcutsKey: #classListMenuSpec;
					adoptWidgetsColor: changeSorterWindow windowColor;
					layoutSpec: aMorph layoutSpec;
					yourself.
				aMorph owner replaceSubmorph: aMorph by: newList.
			].
	]
]!
Smalltalk garbageCollect!

