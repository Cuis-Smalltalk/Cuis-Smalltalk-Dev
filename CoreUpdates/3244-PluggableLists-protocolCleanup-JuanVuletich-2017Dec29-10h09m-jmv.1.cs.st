'From Cuis 5.0 of 7 November 2016 [latest update: #3243] on 29 December 2017 at 10:30:48 am'!

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 10:29:16'!
privateVisualSelection: item
	"Called internally to set a new selection.
	Does not update model"

	self privateVisualSelectionIndex: (self indexForItem: item)! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 10:19:02'!
privateVisualSelectionIndex: idx
	"Called internally to select the index-th item.
	Does not update model"
	self selectedMorph: (self listMorphAt: idx).
	self scrollSelectionIntoView! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 09:42:44'!
visualSelectionIndex
	^scroller submorphs indexOf: selectedMorph! !

!HierarchicalListMorph methodsFor: 'private' stamp: 'jmv 12/29/2017 10:28:53'!
indexForItem: item
	| i |
	item ifNil: [
		^ 0 ].
	i _ scroller submorphs findFirst: [ :m | m complexContents == item ].
	i > 0 ifTrue: [
		^ i ].
	i _ scroller submorphs findFirst: [ :m | m withoutListWrapper = item withoutListWrapper ].
	^ i! !


!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 12/29/2017 09:32:10'!
setSelectionIndex: anInteger
	"Change the model's selected item index to be anInteger."

	setIndexSelector ifNotNil: [
		model perform: setIndexSelector with: anInteger.
		self update: getIndexSelector.
		^ true ].
	^ false! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 09:05:26'!
privateVisualSelection: item
	"Called internally to set a new selection.
	Does not update model"

	self privateVisualSelectionIndex: (list indexOf: item)! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 08:57:30'!
privateVisualSelectionIndex: index
	"Called internally to select the index-th item.
	Does not update model"
	| row |
	row _ index ifNil: [ 0 ].
	row _ row min: self getListSize.  "make sure we don't select past the end"
	self listMorph selectedRow: row.
	self scrollSelectionIntoView! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 08:59:40'!
visualSelection 
	self visualSelectionIndex = 0 ifTrue: [ ^nil ].
	list ifNotNil: [ ^list at: self visualSelectionIndex ].
	^ self getListItem: self visualSelectionIndex! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 08:54:40'!
visualSelectionIndex
	"return the index we have currently selected, or 0 if none"
	^self listMorph selectedRow ifNil: [ 0 ]! !


!PluggableListMorphByItem methodsFor: 'model access' stamp: 'jmv 12/29/2017 09:32:13'!
setSelectionIndex: anInteger
	"Change the model's selected item to be the one at the given index."

	| item |
	setIndexSelector ifNotNil: [
		item _ (anInteger = 0 ifTrue: [nil] ifFalse: [itemList at: anInteger]).
		model perform: setIndexSelector with: item.
		self update: getIndexSelector.
		^ true ].
	^false
	! !


!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 12/29/2017 09:18:40'!
arrowKey: aChar
	"Handle a keyboard navigation character. Answer true if handled, false if not."
	| keyEvent answer nextSelection oldSelection |
	answer := false.
	keyEvent := aChar numericValue.
	oldSelection := self visualSelectionIndex.
	nextSelection := oldSelection.
	keyEvent = 31 ifTrue: [
		"down"
		nextSelection := oldSelection + 1.
		nextSelection > self maximumSelection ifTrue: [ nextSelection := self minimumSelection ]].
	keyEvent = 30 ifTrue: [
		"up"
		nextSelection := oldSelection - 1.
		nextSelection < self minimumSelection ifTrue: [ nextSelection := self maximumSelection ]].
	keyEvent = 1 ifTrue: [ "home"
		nextSelection := 1 ].
	keyEvent = 4 ifTrue: [ "end"
		nextSelection := scroller submorphs size ].
	keyEvent = 11 ifTrue: [ "page up"
		nextSelection := oldSelection - self numSelectionsInView max: 1 ].
	keyEvent = 12 ifTrue: [ "page down"
		nextSelection := oldSelection + self numSelectionsInView ].
	keyEvent = 29 ifTrue: [
		"right"
		selectedMorph ifNotNil: [
			(selectedMorph canExpand and: [ selectedMorph isExpanded not ])
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					self scrollSelectionIntoView.
					answer := true ]
				ifFalse: [ nextSelection := oldSelection + 1 ]]].
	keyEvent = 28 ifTrue: [
		"left"
		selectedMorph ifNotNil: [
			selectedMorph isExpanded
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					self scrollSelectionIntoView.
					answer := true ]
				ifFalse: [
					oldSelection > self minimumSelection ifTrue: [
						nextSelection _ (oldSelection-1 to: 1 by: -1) detect: [ :i | ( scroller submorphs at: i) indentLevel < selectedMorph indentLevel ]].
					]]].
	nextSelection = oldSelection ifFalse: [
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).
		"Update the model in next world cycle, so user gets the immediate feedback."
		UISupervisor whenUIinSafeState: [ self setSelectionIndex: nextSelection ].
		answer := true ].
	^ answer.! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 12/29/2017 10:15:47'!
setSelectionIndex: idx
	"Change the model's selected item index to be anInteger."

	^self setSelectedMorph: (self listMorphAt: idx)! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 10:30:10'!
selection: item
	"Called to set a new selection.
	Updates both model and view."
	"Assumes scroller submorphs is exactly our list.
	Note: MAY NOT work right if list includes repeated items"

	self selectionIndex: (self indexForItem: item)! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 10:23:15'!
selectionIndex: anInteger
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	(self setSelectionIndex: anInteger) ifFalse: [
		self privateVisualSelectionIndex: anInteger ]! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 10:16:19'!
setSelectedMorph: aMorph
	setSelectionSelector ifNil: [ ^ false ].
	model 
		perform: setSelectionSelector
		with: aMorph complexContents	."leave last wrapper in place"
	^ true

 ! !

!HierarchicalListMorph methodsFor: 'updating' stamp: 'jmv 12/29/2017 10:20:49'!
update: aSymbol
	super update: aSymbol.
	aSymbol == getSelectionSelector 
		ifTrue: [
			self privateVisualSelection: self getCurrentSelectionItem.
			^self ].
	aSymbol == getListSelector 
		ifTrue: [
			self list: self getList.
			^self ].

	"Indeed not pretty"
	( aSymbol notEmpty and: [aSymbol first == #openPath]) 
		ifTrue: [
			^(scroller submorphs at: 1 ifAbsent: [^self]) 
				openPath: aSymbol allButFirst adaptor: #asString compare: #=]! !

!HierarchicalListMorph methodsFor: 'private' stamp: 'jmv 12/29/2017 10:20:41'!
insertNewMorphs: morphList

	scroller addAllMorphs: morphList.
	scroller adjustExtent.
	self setScrollDeltas.
	self privateVisualSelection: self getCurrentSelectionItem! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 12/29/2017 09:43:57'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	| index |
	doubleClickSelector ifNil: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index _ self rowAtLocation: localEventPosition.
	index = 0 ifTrue: [ ^super doubleClick: aMouseButtonEvent localPosition: localEventPosition ].
	index == self visualSelectionIndex
		ifFalse: [ self setSelectionIndex: index ].
	^ self model perform: doubleClickSelector! !

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 12/29/2017 09:44:01'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	"The mouse came up within the list; take appropriate action"

	| row |
	row _ self rowAtLocation: localEventPosition.
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [ ^ self ]].
	(autoDeselect == false and:  [row = 0 ]) ifTrue: [ ^ self ].  "work-around the no-mans-land bug"
	"No change if model is locked"
	(autoDeselect and: [ row == self visualSelectionIndex ])
		ifTrue: [
			aMouseButtonEvent mouseButton1Changed ifTrue: [
				self setSelectionIndex: 0 ]]
		ifFalse: [ self setSelectionIndex: row ]! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 12/29/2017 09:38:13'!
arrowKey: aChar
	"Process the up and down arrows in a list pane.  Note that the listView tells us what index variable, how to get the list, and how to move the index."
	| keyEvent oldSelection nextSelection max min howMany answer w |
	answer _ false.
	keyEvent _ aChar numericValue.
	oldSelection _ self getCurrentSelectionIndex.
	nextSelection _ oldSelection.
	max _ self maximumSelection.
	min _ self minimumSelection.
	howMany _ self numSelectionsInView.	"get this exactly??"

	keyEvent = 31 ifTrue: [
		"down-arrow; move down one, wrapping to top if needed"
		nextSelection _ oldSelection + 1.
		nextSelection > max ifTrue: [ nextSelection _ 1 ]].

	keyEvent = 30 ifTrue: [
		"up arrow; move up one, wrapping to bottom if needed"
		nextSelection _ oldSelection - 1.
		nextSelection < 1 ifTrue: [ nextSelection _ max ]].

	keyEvent = 1 ifTrue: [ nextSelection _ 1 ].			"home"
	keyEvent = 4 ifTrue: [ nextSelection _ max ].		"end"
	keyEvent = 11 ifTrue: [ nextSelection _ min max: oldSelection - howMany ].		"page up"
	keyEvent = 12 ifTrue: [ nextSelection _ oldSelection + howMany min: max ].		"page down"
	nextSelection = oldSelection ifFalse: [
		w _ self owningWindow.
		(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.

			"Update the model in next world cycle, so user gets the immediate feedback."
			UISupervisor whenUIinSafeState: [
				self setSelectionIndex: nextSelection ].
			answer _ true ]].
	^answer! !

!PluggableListMorph methodsFor: 'menus' stamp: 'jmv 12/29/2017 09:50:46'!
copySelectionToClipboard
	"Copy my selected item to the clipboard as a string"

	self visualSelection 
		ifNotNil: [ :sel |
			Clipboard storeObject: sel asString ]
		ifNil: [
			self flash ]! !

!PluggableListMorph methodsFor: 'model access' stamp: 'jmv 12/29/2017 09:38:18'!
keyboardSearch: aChar 
	| oldSelection nextSelection max milliSeconds nextSelectionList nextSelectionText |
	nextSelection _ oldSelection _ self getCurrentSelectionIndex.
	max _ self maximumSelection.
	milliSeconds _ Time localMillisecondClock.
	milliSeconds - lastKeystrokeTime > 300 ifTrue: ["just use the one current character for selecting"
		lastKeystrokes _ ''].
	lastKeystrokes _ lastKeystrokes , aChar asLowercase asString.
	lastKeystrokeTime _ milliSeconds.
	nextSelectionList _ OrderedCollection newFrom: (list copyFrom: oldSelection + 1 to: max).
	nextSelectionList addAll: (list copyFrom: 1 to: oldSelection).
	"Get rid of blanks and style used in some lists"
	nextSelectionText _ nextSelectionList detect: [:a | a asString withBlanksTrimmed asLowercase beginsWith: lastKeystrokes]
				ifNone: [^ self flash"match not found"].
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].
	nextSelection _ list findFirst: [:a | a == nextSelectionText].
	"No change if model is locked"
	oldSelection == nextSelection ifTrue: [^ self flash].
	^ self setSelectionIndex: nextSelection! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 10:05:11'!
selection: item
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	self selectionIndex: (list indexOf: item)! !

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 12/29/2017 09:32:29'!
selectionIndex: anInteger
	"Public. Call to set selection.
	Usually, view is updated from model updates.
	If model updating fails (no model index setter defined) then just update visuals."

	(self setSelectionIndex: anInteger) ifFalse: [
		self privateVisualSelectionIndex: anInteger ]! !

!PluggableListMorph methodsFor: 'updating' stamp: 'jmv 12/29/2017 09:46:24'!
update: aSymbol 
	"Refer to the comment in View|update:."

	super update: aSymbol.
	aSymbol == getListSelector ifTrue: [
		self updateList.
		^ self].
	aSymbol == getIndexSelector ifTrue: [
		self privateVisualSelectionIndex: self getCurrentSelectionIndex ]! !

!PluggableListMorph methodsFor: 'updating' stamp: 'jmv 12/29/2017 09:46:28'!
updateList
	| index |
	"the list has changed -- update from the model"
	self getList.
	self listMorph listChanged.
	self setScrollDeltas.
	index _ self getCurrentSelectionIndex.
	self privateVisualSelectionIndex: index! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 12/29/2017 09:38:30'!
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
			dragOnOrOff
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

!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 12/29/2017 09:38:32'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"The mouse has moved, as characterized by the event provided.  Adjust the scrollbar, and alter the selection as appropriate"

	| oldIndex oldVal row |
	row _ (localEventPosition y < 0 and: [ scrollBar scrollValue > 0.0 ])
		ifTrue: [
			scrollBar scrollUp: 1.
			"Leave at least one visible item unaffected, for better visual feedback to the user."
			(self rowAtLocation: `0@0`) + 2 ]
		ifFalse: [
			(localEventPosition y > extent y and: [ scrollBar scrollValue < 1.0 ])
				ifTrue: [
					scrollBar scrollDown: 1.
					"Leave at least one visible item unaffected, for better visual feedback to the user."
					(self rowAtLocation: 0@extent y) - 3 ]
				ifFalse: [ self rowAtLocation: localEventPosition ]].
	row = 0 ifTrue: [ ^ self ].

	"No change if model is locked"
	self owningWindow ifNotNil: [ :w |
		w okToChange ifFalse: [^ self]].

	dragOnOrOff ifNil: [
		"Was not set at mouse down, which means the mouse must have gone down in an area where there was no list item"
		dragOnOrOff _ (self listSelectionAt: row) not.
		dragStartRow _ row ].

	"Set meaning for subsequent dragging of selection"
	oldIndex _ self getCurrentSelectionIndex.
	oldIndex ~= 0 ifTrue: [ oldVal _ self listSelectionAt: oldIndex ].

	"Set or clear new primary selection (listIndex)"
	dragOnOrOff 
		ifTrue: [ self setSelectionIndex: row ]
		ifFalse: [ self setSelectionIndex: 0 ].

	"Need to restore the old one, due to how model works, and set new one."
	oldIndex ~= 0 ifTrue: [ self listSelectionAt: oldIndex put: oldVal ].
	
	"Select all in between if drag was too fast"
	"self listSelectionAt: row put: dragOnOrOff."
	(row min: dragStartRow) to: (row max: dragStartRow) do: [ :r |
		self listSelectionAt: r put: dragOnOrOff ]! !

!PluggableListMorphOfMany methodsFor: 'updating' stamp: 'jmv 12/29/2017 09:46:31'!
update: aSymbol 
	super update: aSymbol.
	aSymbol == #allSelections ifTrue: [
		self privateVisualSelectionIndex: self getCurrentSelectionIndex.
		self redrawNeeded]! !

!methodRemoval: PluggableListMorphByItem #changeModelSelection:!
PluggableListMorphByItem removeSelector: #changeModelSelection:!
!methodRemoval: PluggableListMorph #changeModelSelection:!
PluggableListMorph removeSelector: #changeModelSelection:!
!methodRemoval: PluggableListMorph #getListSelector!
PluggableListMorph removeSelector: #getListSelector!
!methodRemoval: PluggableListMorph #selection!
PluggableListMorph removeSelector: #selection!
!methodRemoval: PluggableListMorph #selectionIndex!
PluggableListMorph removeSelector: #selectionIndex!
!methodRemoval: HierarchicalListMorph #getSelectionIndex!
HierarchicalListMorph removeSelector: #getSelectionIndex!
