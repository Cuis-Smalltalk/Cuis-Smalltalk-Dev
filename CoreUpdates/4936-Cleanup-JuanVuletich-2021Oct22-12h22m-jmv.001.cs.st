'From Cuis 5.0 [latest update: #4935] on 22 October 2021 at 12:23:16 pm'!

!PluggableListMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2021 11:57:37'!
rowAtLocation: aPoint
	"Return the row at the given point or 0 if outside"

	^scroller rowAtLocation: (scroller internalize: aPoint)! !

!PluggableListMorph methodsFor: 'accessing' stamp: 'jmv 10/22/2021 11:57:48'!
rowAtLocation: aPoint ifNone: aNoneBlock 

	^scroller rowAtLocation: (scroller internalize: aPoint) ifNone: aNoneBlock! !

!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 10/22/2021 11:57:11'!
mouseButton2Activity
	scroller highlightedRow: nil.
	super mouseButton2Activity! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 10/22/2021 11:56:47'!
dragEvent: aMouseEvent localPosition: localEventPosition

	| row dragged listItem sm |
	scroller highlightedRow: nil.
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem _ self getListItem: row.
	
	sm _ LabelMorph contents: listItem.
	dragged _ DraggingGuideMorph new.
	dragged addMorph: sm.
	dragged morphExtent: sm morphExtent.
	dragged setProperty: #dragSource toValue: self.
	dragged setProperty: #dropSelectorArgument toValue: listItem.
	
	aMouseEvent hand attachMorphBeside: dragged.! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 10/22/2021 11:56:57'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		scroller noSelection].
	row _ self 
		rowAtLocation: localEventPosition
		ifNone:  [^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

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
		dragSel: (self itemsAreDraggable ifTrue: [ #dragEvent:localPosition: ] ifFalse: [ nil ])! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 10/22/2021 11:57:13'!
mouseLeave: event
	super mouseLeave: event.
	scroller highlightedRow: nil! !

!PluggableListMorph methodsFor: 'events-processing' stamp: 'jmv 10/22/2021 11:57:19'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Reimplemented because we really want #mouseMove when a morph is dragged around"

	aMouseMoveEvent wasHandled ifTrue: [ ^self ]. "not interested"
	scroller highlightedRow: (
		(self viewableArea containsPoint: localEventPosition) ifTrue: [
			self rowAtLocation: localEventPosition ifNone: []]).
	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	aMouseMoveEvent wasHandled: true.
	self mouseMove: aMouseMoveEvent localPosition: localEventPosition.
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue:[
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ]! !

!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 10/22/2021 11:56:49'!
font

	^ scroller font
! !

!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 10/22/2021 11:56:52'!
font: aFontOrNil
	scroller font: aFontOrNil.
! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 10/22/2021 11:57:16'!
privateVisualSelectionIndex: index
	"Called internally to select the index-th item.
	Does not update model"
	| row |
	row _ index ifNil: [ 0 ].
	row _ row min: self getListSize.  "make sure we don't select past the end"
	scroller selectedRow: row.
	self scrollSelectionIntoView! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 10/22/2021 12:10:55'!
visualSelectionIndex
	"return the index we have currently selected, or 0 if none"
	^scroller selectedRow ifNil: [ 0 ]! !

!PluggableListMorph methodsFor: 'updating' stamp: 'jmv 10/22/2021 12:10:51'!
updateList
	| index |
	"the list has changed -- update from the model"
	self getList.
	scroller listChanged.
	self setScrollDeltas.
	index _ self getCurrentSelectionIndex.
	self privateVisualSelectionIndex: index! !

!PluggableListMorph methodsFor: 'private' stamp: 'jmv 10/22/2021 11:56:44'!
changeSelectionTo: nextSelection

	nextSelection = self getCurrentSelectionIndex ifFalse: [
		| window |
		window _ self owningWindow.
		(window isNil or: [ window okToChangeDueTo: self ]) ifTrue: [
			"No change if model is locked"
			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			scroller highlightedRow: nextSelection.
			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ].
		]
	]! !

!PluggableListMorph methodsFor: 'scrolling' stamp: 'jmv 10/22/2021 12:22:50'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar internalScrollValue: scrollBar scrollValue ]
		ifFalse: [
			self flag: #jmvVer2.
			r _ scroller drawBoundsForRow: row.
			r _ ((scroller externalize: r origin) extent: r extent).
			self scrollToShow: r ].
	self scrollMySiblings
! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 10/22/2021 12:11:06'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
	scroller highlightedRow: row.

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

!methodRemoval: PluggableListMorph #listMorph stamp: 'jmv 10/22/2021 12:22:56'!
PluggableListMorph removeSelector: #listMorph!
