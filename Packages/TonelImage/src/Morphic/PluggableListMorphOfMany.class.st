"
A variant of its superclass that allows multiple items to be selected simultaneously.  There is still a distinguished element which is selected, but each other element in the list may be flagged on or off.

"
Class {
	#name : 'PluggableListMorphOfMany',
	#superclass : 'PluggableListMorph',
	#instVars : [
		'dragOnOrOff',
		'getSelectionListSelector',
		'setSelectionListSelector',
		'dragStartRow'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'instance creation' }
PluggableListMorphOfMany class >> withModel: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel [
	^self new
		model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel
]

{ #category : 'testing' }
PluggableListMorphOfMany >> allowsMultipleSelection [
	^true
]

{ #category : 'model access' }
PluggableListMorphOfMany >> itemSelectedAmongMultiple: index [
	^self listSelectionAt: index
]

{ #category : 'model access' }
PluggableListMorphOfMany >> listSelectionAt: index [
	getSelectionListSelector ifNil:[^false].
	^model perform: getSelectionListSelector with: index
]

{ #category : 'model access' }
PluggableListMorphOfMany >> listSelectionAt: index put: value [
	setSelectionListSelector ifNil:[^false].
	^model perform: setSelectionListSelector with: index with: value
]

{ #category : 'initialization' }
PluggableListMorphOfMany >> model: anObject listGetter: listSel primarySelectionGetter: getSelectionSel primarySelectionSetter: setSelectionSel listSelectionGetter: getListSel listSelectionSetter: setListSel [
	"setup a whole load of pluggability options"
	getSelectionListSelector := getListSel.
	setSelectionListSelector := setListSel.
	self model: anObject listGetter: listSel indexGetter: getSelectionSel indexSetter: setSelectionSel
]

{ #category : 'events' }
PluggableListMorphOfMany >> mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition [

	| oldIndex oldVal row w |

	row := self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update
	the view. Model update will be done on mouse button up, so this feedback will be visible before that."
	scroller highlightedRow: row.

	w := self owningWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff := (self listSelectionAt: row) not.
		dragStartRow := row.
		oldIndex := self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal := self listSelectionAt: oldIndex].

		"Different from PluggableListMorph. There, we highlight on mouse down, and act on mouse up.
		Here, we act on mouse down, because we support dragging of selection, so mouse up will
		only happen after user is finished dragging. In order to get the highlight visible for the user,
		update the model on next world cycle."
		UISupervisor whenUIinSafeState: [
			"Set or clear new primary selection (listIndex)"
			dragOnOrOff == true
				ifTrue: [self setSelectionIndex: row]
				ifFalse: [self setSelectionIndex: 0].
			"Need to restore the old one, due to how model works, and set new one."
			oldIndex ~= 0 ifTrue: [self listSelectionAt: oldIndex put: oldVal].
			self listSelectionAt: row put: dragOnOrOff ].
	].
	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: nil
		dblClkSel: #doubleClick:localPosition:
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: (
			aMouseButtonEvent shiftPressed ifFalse: [
				(self valueOfProperty: #allowsItemDrag) = true
						ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ] ])
]

{ #category : 'events' }
PluggableListMorphOfMany >> mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition [

	dragOnOrOff := false.  "So improperly started drags will have not effect"
	dragStartRow := nil
]

{ #category : 'events' }
PluggableListMorphOfMany >> mouseMove: aMouseMoveEvent localPosition: localEventPosition [
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	
	scroller highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: nil]).
	
	aMouseMoveEvent anyButtonPressed ifFalse: [
		^self ].
	
	row := (localEventPosition y < 0 and: [ scrollBar scrollValue > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: `0@0`) + 2 ]
		ifFalse: [
			(localEventPosition y > extent y and: [ self vLeftoverScrollRange > 0 and: [ scrollBar scrollValue < 1.0 ]])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: 0@extent y) - 3 ]
				ifFalse: [
					self rowAtLocation: localEventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff := (self listSelectionAt: row) not.
		dragStartRow := row ].

	"Set meaning for subsequent dragging of selection"
	oldIndex := self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal := self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self setSelectionIndex: row ]
		ifFalse: [ self setSelectionIndex: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	dragStartRow ifNotNil: [
		(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
			self listSelectionAt: r put: dragOnOrOff ]].
]

{ #category : 'updating' }
PluggableListMorphOfMany >> update: aSymbol  [
	super update: aSymbol.
	aSymbol == #allSelections ifTrue: [
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		self redrawNeeded]
]
