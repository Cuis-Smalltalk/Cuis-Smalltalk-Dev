'From Cuis7.5 [latest update: #7552] on 11 September 2025 at 11:39:39 am'!

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 9/9/2025 20:23:03'!
newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any.
	If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	keyboardFocus == aMorphOrNil ifFalse: [
		oldFocus := keyboardFocus.
		keyboardFocus := aMorphOrNil.
		oldFocus ifNotNil: [oldFocus keyboardFocusChange: false].
		aMorphOrNil ifNotNil: [aMorphOrNil keyboardFocusChange: true]].! !

!Morph methodsFor: 'event handling testing' stamp: 'jmv 9/11/2025 10:27:42'!
clickToFocusConsumesEvent
	"	For instance, for text editors and lists, if (Preferences at: #focusFollowsMouse) not, the click event
	used to set keyboard focus should not be used for anything else. Cursor position, selection, etc are not affected.
	 	For other morphs, like buttons, menu items, and the World, click may set keyboard focus, but the click action
	is also desired.
		Answer true by default. Morphs that want to react to the setting-focus click should redefine this method
	answering false. See inheritance."

	^true! !


!HaloHandleMorph methodsFor: 'event handling testing' stamp: 'jmv 9/11/2025 10:27:49'!
clickToFocusConsumesEvent
	"We react immediately to clicks, even if they are used to set keyboard focus on us.
	See inheritance."
	
	^false! !


!MenuItemMorph methodsFor: 'event handling testing' stamp: 'jmv 9/11/2025 10:27:33'!
clickToFocusConsumesEvent
	"We react immediately to clicks, even if they are used to set keyboard focus on us.
	See inheritance."
	
	^false! !


!PluggableButtonMorph methodsFor: 'event handling testing' stamp: 'jmv 9/11/2025 10:28:00'!
clickToFocusConsumesEvent
	"We react immediately to clicks, even if they are used to set keyboard focus on us.
	See inheritance."
	
	^false! !


!PasteUpMorph methodsFor: 'event handling testing' stamp: 'jmv 9/11/2025 10:28:08'!
clickToFocusConsumesEvent
	"We react immediately to clicks, even if they are used to set keyboard focus on us.
	See inheritance."
	
	^false! !


!LayoutAdjustingMorph methodsFor: 'event handling testing' stamp: 'jmv 9/11/2025 10:28:16'!
clickToFocusConsumesEvent
	"We react immediately to clicks, even if they are used to set keyboard focus on us.
	See inheritance."
	
	^false! !


!ResizeMorph methodsFor: 'event handling testing' stamp: 'jmv 9/11/2025 10:28:43'!
clickToFocusConsumesEvent
	"We react immediately to clicks, even if they are used to set keyboard focus on us.
	See inheritance."
	
	^false! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 10:14:07'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	
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

	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
	
	"If we click to focus, don't process this event as a regular click.
	See also #processMouseUp:localPosition:, where the focus is actually set."
	(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifFalse: [ self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue: [
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 11:38:31'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	
	"If we click to focus, mouse down wasn't processed as a regular event in #processMouseDown:localPosition:,
	because we'll set keyboard focus on mouse up. Then, also ignore mouse moves.
	See also #processMouseUp:localPosition:, where the focus is actually set."
	(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifFalse: [ self mouseMove: aMouseMoveEvent localPosition: localEventPosition ].
	
	
	
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 10:13:24'!
processMouseUp: aMouseButtonEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseButtonEvent hand releaseMouseFocus: self.
	aMouseButtonEvent wasHandled: true.
	aMouseButtonEvent mouseButton3Changed ifTrue: [ 
		self mouseButton3Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	aMouseButtonEvent mouseButton2Changed ifTrue: [
		self mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	"If we click to focus, don't process this event as a regular click.
	See also #processMouseDown:localPosition:"
	(self clickToFocusConsumesEvent and: [
		(Preferences at: #focusFollowsMouse) not and: [
			self hasKeyboardFocus not ]])
		ifTrue: [ aMouseButtonEvent hand newKeyboardFocus: self ]
		ifFalse: [ self mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition ].

	self stopStepping: #processMouseStillDown! !


!EntryFieldMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 17:59:00'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	self editor mouseDown: aMouseButtonEvent index: (self characterIndexAtPoint: localEventPosition).
	self redrawNeeded.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 17:59:04'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
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


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/11/2025 09:44:19'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	row := self 
		rowAtLocation: localEventPosition
		ifNone:  [
			super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.
			^self ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update
	the view. Model update will be done on mouse button up, so this feedback will be visible before that."
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


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 9/11/2025 09:45:10'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

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
						ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ] ])! !


!InnerTextMorph methodsFor: 'events' stamp: 'jmv 9/9/2025 18:00:05'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Make this TextMorph be the keyboard input focus, if it isn't already,
		and repond to the text selection gesture."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	self handleInteraction: [ editor mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: nil
		clkNHalf: #clickAndHalf:localPosition:
		dblClkSel: nil
		dblClkNHalfSel: #doubleClickAndHalf:localPosition:
		tripleClkSel: nil! !

