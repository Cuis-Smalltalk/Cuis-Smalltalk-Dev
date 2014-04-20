'From Cuis 4.2 of 25 July 2013 [latest update: #1969] on 7 March 2014 at 11:44:43.316819 pm'!

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 3/7/2014 23:08'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	self highlightedMorph: itemMorph.
	(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 3/7/2014 23:40'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	highlightedMorph ifNil: [ ^self ].
	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect and: [ highlightedMorph == selectedMorph ])
		ifTrue: [ self setSelectedMorph: nil ]
		ifFalse: [ self setSelectedMorph: highlightedMorph ].
	self highlightedMorph: nil! !

!HierarchicalListMorph methodsFor: 'initialization' stamp: 'jmv 11/14/2011 10:40'!
model: anObject listGetter: getListSel indexGetter: getSelectionSel indexSetter: setSelectionSel mainView: aMainView menuGetter: getMenuSel keystrokeAction: keyActionSel

	self model: anObject.
	getListSelector _ getListSel.
	getSelectionSelector _ getSelectionSel.
	setSelectionSelector _ setSelectionSel.
	mainView _ aMainView.
	menuGetter _ getMenuSel.
	keystrokeActionSelector _ keyActionSel.
	autoDeselect _ true.
	self list: self getList.! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 3/7/2014 23:23'!
selectedMorph: aMorph

	selectedMorph ifNotNil: [
		selectedMorph isSelected: false ].
	selectedMorph _ aMorph.
	selectedMorph ifNotNil: [
		selectedMorph isSelected: true ]! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 3/7/2014 23:41'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [ ^ self ]].
	(autoDeselect == false and:  [row = 0 ]) ifTrue: [ ^ self ].  "work-around the no-mans-land bug"
	"No change if model is locked"
	(autoDeselect and: [ row == self selectionIndex ])
		ifTrue: [
			aMouseButtonEvent mouseButton1Changed ifTrue: [
				self changeModelSelection: 0 ]]
		ifFalse: [ self changeModelSelection: row ]! !

!methodRemoval: HierarchicalListMorph #autoDeselect!
HierarchicalListMorph removeSelector: #autoDeselect!
