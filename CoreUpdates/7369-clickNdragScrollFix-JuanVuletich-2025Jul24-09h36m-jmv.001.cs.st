'From Cuis7.5 [latest update: #7367] on 24 July 2025 at 9:54:25 am'!

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 7/24/2025 09:53:36'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
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

