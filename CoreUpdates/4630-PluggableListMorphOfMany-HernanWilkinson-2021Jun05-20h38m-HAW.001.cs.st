'From Cuis 5.0 [latest update: #4532] on 5 June 2021 at 8:39:29 pm'!

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'HAW 6/5/2021 20:39:06'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
	self listMorph highlightedRow: row.

	w _ self owningWindow.
	(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

		"Set meaning for subsequent dragging of selection"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row.
		oldIndex _ self getCurrentSelectionIndex.
		oldIndex ~= 0 ifTrue: [oldVal _ self listSelectionAt: oldIndex].

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
		tripleClkSel: nil! !

