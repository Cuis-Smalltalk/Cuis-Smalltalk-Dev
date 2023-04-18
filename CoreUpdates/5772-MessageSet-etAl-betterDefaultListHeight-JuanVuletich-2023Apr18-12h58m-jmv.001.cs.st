'From Cuis 6.0 [latest update: #5771] on 18 April 2023 at 12:59:26 pm'!

!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 4/17/2023 16:54:23'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	| listPropHeight |
	listPropHeight := model messageList size / 32.0 min: 0.4.
	self layoutMorph
		addMorph: self buildMorphicMessageList proportionalHeight: listPropHeight;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 - listPropHeight.
	model changed: #editSelection! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 4/17/2023 16:44:21'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPropHeight listPane |
	listPropHeight := model list size / 32.0 min: 0.4.
	listPane := PluggableListMorphOfMany
		model: model
		listGetter: #list
		primarySelectionGetter: #listIndex
		primarySelectionSetter: #toggleListIndex:
		listSelectionGetter: #listSelectionAt:
		listSelectionSetter: #listSelectionAt:put:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	
	self layoutMorph
		addMorph: listPane proportionalHeight: listPropHeight;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 - listPropHeight.! !


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 4/17/2023 16:44:58'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPropHeight listPane |
	listPropHeight := model list size / 32.0 min: 0.4.
	listPane := PluggableListMorph
		model: model
		listGetter: #list
		indexGetter: #listIndex
		indexSetter: #toggleListIndex:
		mainView: self
		menuGetter: #listMenu
		keystrokeAction: #changeListKey:from:.
	
	self layoutMorph
		addMorph: listPane proportionalHeight: listPropHeight;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 - listPropHeight.! !

