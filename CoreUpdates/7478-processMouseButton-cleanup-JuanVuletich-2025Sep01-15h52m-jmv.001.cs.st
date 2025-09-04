'From Cuis7.5 [latest update: #7475] on 1 September 2025 at 3:58:47 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/1/2025 15:33:21'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."
	aMouseButtonEvent wasHandled ifTrue: [ ^self ]. "not interested"
	aMouseButtonEvent hand removePendingBalloonFor: self.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue:[
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/1/2025 15:40:18'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"System level event handling."

	aMouseButtonEvent wasHandled ifTrue: [^self]. "not interested"
	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	
	self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition.
	self stopStepping: #processMouseStillDown! !


!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/1/2025 15:35:14'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	aMouseButtonEvent shiftPressed ifTrue: [
		super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].  "enable label editing" 
	aMouseButtonEvent hand newMouseFocus: owner. "Redirect to menu for valid transitions"
	owner selectItem: self! !

!MenuItemMorph methodsFor: 'events' stamp: 'jmv 9/1/2025 15:38:24'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Create a copy of me or my submenu."

	aMouseButtonEvent shiftPressed ifTrue: [
		(subMenu
			ifNil: [ self duplicate ]
			ifNotNil: [
				(subMenu duplicate)
					addTitle: contents;
					addStayUpIcons;
					stayUp;
					adjustSubmorphsLayout;
					yourself ])
						openInHand ].
	super mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 9/1/2025 15:35:53'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph := self itemFromPoint: localEventPosition.
	itemMorph ifNil: [
		super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	self highlightedMorph: itemMorph.
	(itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)))
		ifTrue: [
			self toggleExpandedState: itemMorph event: aMouseButtonEvent.
			^self ]. 
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ])! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/1/2025 15:36:09'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		scroller noSelection].
	row := self 
		rowAtLocation: localEventPosition
		ifNone:  [
			super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
			^self ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view.
	Model update will be done on mouse button up, so this feedback will be visible before that."
	scroller highlightedRow: row.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil
		dragSel: ((self valueOfProperty: #allowsItemDrag) = true ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ])! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 9/1/2025 15:36:27'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row := self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
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
						ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ] ])! !


!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 9/1/2025 15:39:43'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition

	(self includesPixel: aMouseButtonEvent eventPosition)
		ifTrue: [
			((self upButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ self stillActive; goUp. ^self ].
			((self downButtonPosition extent: ScrollBar scrollbarThickness) containsPoint: localEventPosition)
				ifTrue: [ self stillActive; goDown. ^self ].
			self selected: (localEventPosition y // self itemHeight) +  self firstVisible.
			completer insertSelected ]
		ifFalse: [ self delete. completer menuClosed ]! !

