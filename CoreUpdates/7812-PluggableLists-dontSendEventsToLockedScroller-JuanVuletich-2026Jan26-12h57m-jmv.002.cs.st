'From Cuis7.7 [latest update: #7810] on 26 January 2026 at 2:05:08 pm'!

!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 26/Jan/2026 13:29:57'!
mouseButton2Activity
	scroller isLocked ifFalse: [
		self highlightedMorph: nil ].
	super mouseButton2Activity.! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:59:10'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition

	scroller isLocked ifTrue: [ ^self ].

	doubleClickSelector ifNotNil: [
		^ (self window ifNil: [model]) perform: doubleClickSelector].
	selectedMorph ifNotNil: [
		self toggleExpandedState: selectedMorph.
		self scrollSelectionIntoView ]! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:59:25'!
dragEvent: aMouseEvent localPosition: localEventPosition
	| draggedItem draggingGuide label |

	scroller isLocked ifTrue: [ ^self ].

	draggedItem := ((self itemFromPoint: localEventPosition) ifNil: [ ^self ]) withoutListWrapper.
	label := LabelMorph contents: draggedItem asString.
	draggingGuide := DraggingGuideMorph new.
	draggingGuide addMorph: label.
	draggingGuide morphExtent: label morphExtent.
	draggingGuide setProperty: #dragSource toValue: self.
	draggingGuide setProperty: #dropSelectorArgument toValue: draggedItem.	
	aMouseEvent hand attachMorphBeside: draggingGuide.! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:26:57'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	scroller isLocked ifTrue: [ ^self ].

	(self arrowKey: aKeyboardEvent) ifNotNil: [
		aKeyboardEvent wasHandled: true.
		^self ].

	aKeyboardEvent isEsc ifTrue: [
		" escape key"
		self mouseButton2Activity.
		aKeyboardEvent wasHandled: true ].! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:55:05'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |

	scroller isLocked ifFalse: [
		itemMorph := self itemFromPoint: localEventPosition.
		itemMorph ifNil: [
			super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
			^self ].
		self highlightedMorph: itemMorph.
		(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
			ifTrue: [
				self toggleExpandedState: itemMorph event: aMouseButtonEvent.
				^self ]].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ])! !

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:56:49'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	scroller isLocked ifTrue: [ ^self ].
	highlightedMorph ifNil: [ ^self ].

	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	(autoDeselect and: [ highlightedMorph == selectedMorph ])
		ifTrue: [ self setSelectedMorph: nil ]
		ifFalse: [ self setSelectedMorph: highlightedMorph ].
	self highlightedMorph: nil! !


!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 26/Jan/2026 13:21:27'!
keyboardFocused
	"Just given focus via keyboard.
	Autoselect first row."

	scroller isLocked ifFalse: [
		(scroller selectedRow = 0 and: [
			self allowsMultipleSelection not and: [ self getListSize > 0 ]])
				ifTrue: [ self navigateToTop ]].! !

!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 26/Jan/2026 13:29:54'!
mouseButton2Activity
	scroller isLocked ifFalse: [
		scroller highlightedRow: nil ].
	super mouseButton2Activity.! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 14:00:05'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	scroller isLocked ifTrue: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	doubleClickSelector ifNil: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index := self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [
		^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index == self visualSelectionIndex
		ifFalse: [ self setSelectionIndex: index ].
	^ (self window ifNil: [model]) perform: doubleClickSelector! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 14:00:22'!
dragEvent: aMouseEvent localPosition: localEventPosition
	| row listItem label draggingGuide |
	scroller isLocked ifTrue: [ ^self ].

	scroller highlightedRow: nil.
	row := self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem := self getListItem: row.
	
	label := LabelMorph contents: listItem.
	draggingGuide := DraggingGuideMorph new.
	draggingGuide addMorph: label.
	draggingGuide morphExtent: label morphExtent.
	draggingGuide setProperty: #dragSource toValue: self.
	draggingGuide setProperty: #dropSelectorArgument toValue: listItem.
	draggingGuide setProperty: #dragLocalPos toValue: localEventPosition.
	
	aMouseEvent hand attachMorphBeside: draggingGuide! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:27:52'!
keyStroke: aKeyboardEvent 
	"Process keys"

	| aCharacter |
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	scroller isLocked ifTrue: [ ^self ].
	
	(self arrowKey: aKeyboardEvent) ifNotNil: [
		aKeyboardEvent wasHandled: true.
		^self ].

	"All the cases below consume the event."
	aKeyboardEvent wasHandled: true.
	aKeyboardEvent isReturnKey ifTrue: [
		doubleClickSelector notNil ifTrue: [(self window ifNil: [model]) perform: doubleClickSelector].
		^self].
	aKeyboardEvent isEsc ifTrue: [
		self mouseButton2Activity.
		^self ].
	aKeyboardEvent isDelete ifTrue: [
		self deleteAction.
		^self ].
	aKeyboardEvent isBackspace ifTrue: [
		self deleteAction.
		^self ].
	
	aCharacter := aKeyboardEvent keyCharacter.
	self keyboardSearch: aCharacter! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:55:31'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |

	scroller isLocked ifFalse: [
		row := self 
			rowAtLocation: localEventPosition
			ifNone:  [
				super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
				^self ].
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update
		the view. Model update will be done on mouse button up, so this feedback will be visible before that."
		scroller highlightedRow: row ].

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ])! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:56:56'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	scroller isLocked ifTrue: [ ^self ].

	self clickedAt: localEventPosition maybeDeselect: aMouseButtonEvent mouseButton1Changed.! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 26/Jan/2026 14:03:54'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |

	scroller isLocked ifTrue: [ ^self ].

	scroller highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: nil]).

	eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
	scroller  mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller.! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:56:21'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |

	scroller isLocked ifFalse: [
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
				self listSelectionAt: row put: dragOnOrOff ]]].

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
						ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ] ]).! !

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:57:04'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	scroller isLocked ifTrue: [ ^self ].

	dragOnOrOff := false.  "So improperly started drags will have not effect"
	dragStartRow := nil.! !

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 26/Jan/2026 13:57:27'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |

	scroller isLocked ifTrue: [ ^self ].

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
			self listSelectionAt: r put: dragOnOrOff ]].! !

