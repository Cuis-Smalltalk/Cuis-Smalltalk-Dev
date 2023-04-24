'From Cuis 6.0 [latest update: #5775] on 24 April 2023 at 10:49:32 am'!

!MessageSetWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2023 10:28:42'!
buildMorphicWindow
	"Answer a morphic window with the given label that can display the receiver"

	| listPropHeight |
	listPropHeight := model messageList size + 0.4 / 38 min: 0.4.
	self layoutMorph
		addMorph: self buildMorphicMessageList proportionalHeight: listPropHeight;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 1.0 - listPropHeight.
	model changed: #editSelection! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2023 10:27:52'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPropHeight listPane |
	listPropHeight := model list size + 0.4 / 38.0 min: 0.4.
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


!VersionsBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2023 10:28:06'!
buildMorphicWindow
	"Open a morphic view for the messageSet, whose label is labelString. 
	The listView may be either single or multiple selection type"

	| listPropHeight listPane |
	listPropHeight := model list size + 0.4 / 38.0 min: 0.4.
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

