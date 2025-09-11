'From Cuis7.5 [latest update: #7552] on 11 September 2025 at 11:32:22 am'!

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/11/2025 11:29:24'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	| eventPositionLocalToScroller |

	scroller highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: nil]).

	scroller ifNotNil: [
		eventPositionLocalToScroller := localEventPosition - scroller morphPosition.
		scroller  mouseMove: aMouseMoveEvent localPosition: eventPositionLocalToScroller ].! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/11/2025 10:57:43'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MorphicEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	"Rules say that by default a morph gets #mouseMove iff
		* the hand is not dragging anything,
			+ and some button is down,
			+ and the receiver is the current mouse focus."
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !


!DraggableButtonMorph methodsFor: 'events' stamp: 'jmv 9/11/2025 10:56:13'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent anyButtonPressed ifTrue: [
		dragSelector ifNotNil: [
			model perform: dragSelector with: localEventPosition ]]! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 9/11/2025 11:00:03'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
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
			self listSelectionAt: r put: dragOnOrOff ]].! !


!ResizeMorph methodsFor: 'events' stamp: 'jmv 9/11/2025 10:55:53'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition

	aMouseMoveEvent anyButtonPressed ifTrue: [
		self selectTo: localEventPosition ]! !

!methodRemoval: InnerTextMorph #processMouseMove:localPosition: stamp: 'jmv 9/11/2025 11:30:47'!
InnerTextMorph removeSelector: #processMouseMove:localPosition:!
!methodRemoval: PluggableListMorph #processMouseMove:localPosition: stamp: 'jmv 9/11/2025 11:30:47'!
PluggableListMorph removeSelector: #processMouseMove:localPosition:!
!methodRemoval: HierarchicalListMorph #processMouseMove:localPosition: stamp: 'jmv 9/11/2025 11:30:47'!
HierarchicalListMorph removeSelector: #processMouseMove:localPosition:!
