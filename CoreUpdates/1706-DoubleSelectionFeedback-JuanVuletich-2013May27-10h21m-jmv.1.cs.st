'From Cuis 4.1 of 12 December 2012 [latest update: #1704] on 27 May 2013 at 10:52:50.815786 am'!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView highlightedMorph '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!classDefinition: #IndentingListItemMorph category: #'Morphic-Views for Models'!
StringMorph subclass: #IndentingListItemMorph
	instanceVariableNames: 'indentLevel isExpanded complexContents firstChild container nextSibling isSelected icon isHighlighted '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!classDefinition: #InnerListMorph category: #'Morphic-Views for Models'!
InnerPluggableMorph subclass: #InnerListMorph
	instanceVariableNames: 'listItems font selectedRow selectedRows highlightedRow '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!

!HierarchicalListMorph methodsFor: 'event handling' stamp: 'jmv 5/27/2013 10:49'!
mouseButton2Activity
	self highlightedMorph: nil.
	super mouseButton2Activity! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 5/27/2013 10:42'!
highlightedMorph: aMorph

	highlightedMorph ifNotNil: [
		highlightedMorph isHighlighted: false ].
	highlightedMorph _ aMorph.
	highlightedMorph ifNotNil: [
		highlightedMorph isHighlighted: true ]! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 5/27/2013 10:15'!
listMorphAt:  idx
	"Called internally to select the index-th item."
	| theMorph index |
	idx ifNil: [^ self].
	index _ idx min: scroller submorphs size max: 0.
	theMorph _ index = 0 ifTrue: [ nil ] ifFalse: [ scroller submorphs at: index ].
	^theMorph! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'jmv 5/27/2013 10:00'!
isHighlighted: aBoolean

	isHighlighted _ aBoolean.
	self redrawNeeded! !


!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 5/27/2013 09:26'!
drawHighlightOn: aCanvas
"Ademas, poner el mouse pointer de relojito si tarda... Detectarlo automaticamente, etc. Aunque no estoy seguro de como hacerlo... quizas colgar un cachito de codigo en un proceso de mayor prioridad, onda 'si pasa 1 segundo, y todavia no te resetee este flag, entonces pone el relojito'"
	| selectionDrawBounds |
	highlightedRow  ifNil: [ ^self ].
	highlightedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: highlightedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus) duller! !

!InnerListMorph methodsFor: 'as yet unclassified' stamp: 'jmv 5/27/2013 10:22'!
highlightedRow: n
	highlightedRow _ n.
	self redrawNeeded! !


!PluggableListMorph methodsFor: 'event handling' stamp: 'jmv 5/27/2013 10:48'!
mouseButton2Activity
	self listMorph highlightedRow: nil.
	super mouseButton2Activity! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 5/27/2013 10:42'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| itemMorph |
	aMouseButtonEvent hand newKeyboardFocus: self.
	itemMorph _ self itemFromPoint: localEventPosition.
	(itemMorph notNil and: [
		itemMorph inToggleArea: (itemMorph internalize: (scroller internalize: localEventPosition)) ])
		ifTrue: [ ^self toggleExpandedState: itemMorph event: aMouseButtonEvent ]. 
	itemMorph ifNil: [ ^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].
	self highlightedMorph: itemMorph.
	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent 
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: nil
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/27/2013 10:42'!
arrowKey: aChar
	"Handle a keyboard navigation character. Answer true if handled, false if not."
	| keyEvent answer nextSelection oldSelection |
	answer _ false.
	keyEvent _ aChar asciiValue.
	oldSelection _ self getSelectionIndex.
	nextSelection _ oldSelection.
	keyEvent = 31 ifTrue: [		"down"
		nextSelection _ oldSelection + 1 ].
	keyEvent = 30 ifTrue: [		"up"
		nextSelection _ (oldSelection - 1 max: 1) ].
	keyEvent = 1 ifTrue: [		"home"
		nextSelection _ 1 ].
	keyEvent = 4 ifTrue: [		"end"
		nextSelection _ scroller submorphs size ].
	keyEvent = 11 ifTrue: [		"page up"
		nextSelection _ (oldSelection - self numSelectionsInView max: 1) ].
	keyEvent = 12 ifTrue: [		"page down"
		nextSelection _ oldSelection + self numSelectionsInView ].
	keyEvent = 29 ifTrue: [		"right"
		selectedMorph ifNotNil: [
			(selectedMorph canExpand and: [ selectedMorph isExpanded not ])
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					answer _ true ]
				ifFalse: [ nextSelection _ oldSelection + 1 ]] ].
	keyEvent = 28 ifTrue: [		"left"
		selectedMorph ifNotNil: [
			selectedMorph isExpanded
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					answer _ true ]
				ifFalse: [ nextSelection _ (oldSelection - 1 max: 1) ]].
		^ true ].
	nextSelection = oldSelection ifFalse: [

		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).

		"Update the model in next world cycle, so user gets the immediate feedback."
		WorldState addDeferredUIMessage: [
			self setSelectionIndex: nextSelection ].
		answer _ true ].
	^ answer! !

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/27/2013 10:16'!
setSelectionIndex: idx
	"Called internally to select the index-th item."
	self setSelectedMorph: (self listMorphAt: idx)! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 5/27/2013 10:43'!
selectedMorph: aMorph

	selectedMorph ifNotNil: [
		selectedMorph isSelected: false ].
	selectedMorph _ aMorph.
	selectedMorph ifNotNil: [
		selectedMorph isSelected: true ].
	self highlightedMorph: nil! !

!HierarchicalListMorph methodsFor: 'selection' stamp: 'jmv 5/27/2013 10:16'!
selectionIndex: idx
	"Called internally to select the index-th item."
	self selectedMorph: (self listMorphAt: idx).
	self scrollSelectionIntoView! !


!IndentingListItemMorph methodsFor: 'accessing' stamp: 'jmv 5/27/2013 09:58'!
isSelected: aBoolean

	isSelected _ aBoolean.
	isHighlighted _ false.
	self redrawNeeded! !

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 5/27/2013 09:59'!
drawOn: aCanvas

	| tRect sRect colorToUse sLeft aForm centeringOffset |
	isHighlighted == true ifTrue: [
		aCanvas
			fillRectangle: (0@0 extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) duller ].
	isSelected ifTrue: [
		aCanvas
			fillRectangle: (0@0 extent: extent)
			color: (Theme current
				listHighlightFocused: owner owner hasKeyboardFocus) ].
	
	complexContents hasContents ifTrue: [
		tRect _ self toggleRectangle.
		aForm _ isExpanded 
			ifTrue: [ container expandedForm ]
			ifFalse: [ container notExpandedForm ].
		centeringOffset _ ((tRect height - aForm extent y) / 2.0) rounded.
		aCanvas 
			image: aForm 
			at: (tRect topLeft translatedBy: 0 @ centeringOffset) ].

	icon isNil 
	ifFalse: [
		 aCanvas 
			image: icon
			at:  (tRect topLeft translatedBy:  icon width  @ centeringOffset).
		sLeft _ indentLevel * 12 + 16 + (icon width + 2).
	]
	ifTrue: [
		sLeft _ indentLevel * 12 + 16.
	].
	sRect _ sLeft@0 extent: extent - (sLeft@0).
	colorToUse _ complexContents preferredColor ifNil: [ color ].

	aCanvas
		drawString: contents asString
		in: sRect
		font: self fontToUse
		color: colorToUse! !

!IndentingListItemMorph methodsFor: 'initialization' stamp: 'jmv 5/27/2013 09:58'!
initialize

	super initialize.
	indentLevel _ 0.
	isExpanded _ false.
	isSelected _ false.
	isHighlighted _ false.
	icon _ nil! !


!InnerListMorph methodsFor: 'initialization' stamp: 'jmv 5/27/2013 09:39'!
initialize
	super initialize.
	self color: Color black.
	font _ Preferences standardListFont.
	listItems _ #().
	selectedRow _ nil.
	selectedRows _ Set new.
	highlightedRow _ nil! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 5/27/2013 09:41'!
selectedRow: index
	"select the index-th row.  if nil, remove the current selection"
	selectedRow _ index.
	highlightedRow _ nil.
	self redrawNeeded! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 5/24/2013 08:54'!
drawOn: aCanvas

	listItems size = 0 ifTrue: [ ^self ].
 
	self drawHighlightOn: aCanvas.
	self drawSelectionOn: aCanvas.

	(self topVisibleRowForCanvas: aCanvas)
		to: (self bottomVisibleRowForCanvas: aCanvas)
		do: [ :row |
			(owner itemSelectedAmongMultiple:  row) ifTrue: [
				self drawBackgroundForMulti: row on: aCanvas. ].
			self draw: (self item: row) asStringOrText atRow: row on: aCanvas ]! !

!InnerListMorph methodsFor: 'drawing' stamp: 'jmv 9/9/2012 23:52'!
drawSelectionOn: aCanvas
	| selectionDrawBounds |
	selectedRow ifNil: [ ^self ].
	selectedRow = 0 ifTrue: [ ^self ].
	selectionDrawBounds _ self drawBoundsForRow: selectedRow.
	selectionDrawBounds _ selectionDrawBounds intersect: (0@0 extent: extent).
	aCanvas
		fillRectangle: selectionDrawBounds
		color: (Theme current listHighlightFocused: owner hasKeyboardFocus)! !

!InnerListMorph methodsFor: 'private' stamp: 'jmv 5/27/2013 09:39'!
noSelection
	selectedRow _ nil.
	selectedRows _ Set new.
	highlightedRow _ nil! !


!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 5/27/2013 10:52'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| ok prevSelection prevScrollValue |
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar value.
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) 
		ifTrue: [^self flash].
	self hasEditingConflicts 
		ifTrue: [
			(self confirm: 
'Caution!! Contents were saved
elsewhere since you started
editing them here.  Accept anyway?' ) 
					ifFalse: [^self flash]].
	ok _ model acceptContentsFrom: owner.
	ok == true
		ifTrue: [ model refetch ].

	"sps 8/13/2001 22:41: restore selection and scroll info"
	self flag: #jmvVer2.	"Check this. Do we need the error handler? Consider explicitly the case where no world?"
	["During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the codeholding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
		ok
			ifTrue: [
				self editor selectFrom: prevSelection first to: prevSelection last.
				WorldState addDeferredUIMessage: [
					self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
					owner setScrollDeltas.
					owner verticalScrollBar setValue: prevScrollValue ]]
	] on: Error do: nil! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 5/27/2013 10:30'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| row |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self.
		"If we are focusing, deselect, so that later selection doesn't result in deselect."
		self listMorph noSelection].
	row _ self rowAtLocation: localEventPosition.
	
	row = 0  ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view.
	Model update will be done on mouse button up, so this feedback will be visible before that."
	self listMorph highlightedRow: row.

	aMouseButtonEvent hand 
		waitForClicksOrDragOrSimulatedMouseButton2: self
		event: aMouseButtonEvent
		clkSel: #click:localPosition:
		clkNHalf: nil
		dblClkSel: (doubleClickSelector ifNotNil: [ #doubleClick:localPosition: ])
		dblClkNHalfSel: nil
		tripleClkSel: nil! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/27/2013 10:32'!
arrowKey: aChar
	"Process the up and down arrows in a list pane.  Note that the listView tells us what index variable, how to get the list, and how to move the index."
	| keyEvent oldSelection nextSelection max min howMany answer w |
	answer _ false.
	keyEvent _ aChar asciiValue.
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
		w _ self ownerThatIsA: SystemWindow.
		(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.

			"Update the model in next world cycle, so user gets the immediate feedback."
			WorldState addDeferredUIMessage: [
				self changeModelSelection: nextSelection ].
			answer _ true ]].
	^answer! !


!PluggableListMorphOfMany methodsFor: 'events' stamp: 'jmv 5/27/2013 10:31'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition

	| oldIndex oldVal row w |
	self hasKeyboardFocus ifFalse: [
		aMouseButtonEvent hand newKeyboardFocus: self ].

	row _ self rowAtLocation: localEventPosition.

	row = 0 ifTrue: [
		^super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ].

	"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
	self listMorph highlightedRow: row.

	w _ self ownerThatIsA: SystemWindow.
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
		WorldState addDeferredUIMessage: [
			"Set or clear new primary selection (listIndex)"
			dragOnOrOff
				ifTrue: [self changeModelSelection: row]
				ifFalse: [self changeModelSelection: 0].
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


!WorldState methodsFor: 'update cycle' stamp: 'jmv 5/27/2013 09:27'!
doOneCycleNowFor: aWorld
	"Immediately do one cycle of the interaction loop.
	This should not be called directly, but only via doOneCycleFor:"

	| hadAnyEvent |
	"See #eventTickler"
	Cursor currentCursor = Cursor wait ifTrue: [
		Cursor normal show ].

	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely: aWorld.

	"Run steps, alarms and deferred UI messages"
	aWorld runStepMethods.

	"Process user input events. Run all event triggered code."
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ h processEventQueue.
		activeHand _ nil ].

	"The default is the primary hand"
	activeHand _ self hands first.


	^hadAnyEvent! !

!classDefinition: #InnerListMorph category: #'Morphic-Views for Models'!
InnerPluggableMorph subclass: #InnerListMorph
	instanceVariableNames: 'listItems font selectedRow selectedRows highlightedRow'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!classDefinition: #IndentingListItemMorph category: #'Morphic-Views for Models'!
StringMorph subclass: #IndentingListItemMorph
	instanceVariableNames: 'indentLevel isExpanded complexContents firstChild container nextSibling isSelected icon isHighlighted'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!methodRemoval: HierarchicalListMorph #highlighted:!
HierarchicalListMorph removeSelector: #highlighted:!
!classDefinition: #HierarchicalListMorph category: #'Morphic-Views for Models'!
PluggableScrollPane subclass: #HierarchicalListMorph
	instanceVariableNames: 'selectedMorph getListSelector keystrokeActionSelector autoDeselect sortingSelector getSelectionSelector setSelectionSelector menuGetter mainView highlightedMorph'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
IndentingListItemMorph allInstancesDo: [ :m | m instVarNamed: 'isHighlighted' put: false]!
