'From Cuis7.5 [latest update: #7546] on 10 September 2025 at 12:03:03 pm'!

!IndentingListItemMorph methodsFor: 'event handling testing' stamp: 'jmv 9/10/2025 12:00:35'!
handlesKeyboardFocus
	"Return true if the receiver wishes to handle keyboard focus.
	Answer false because it is handled by our owner, a subinstance of HierarchicalListMorph"

	^false! !


!MouseOverHandler methodsFor: 'event handling' stamp: 'jmv 9/10/2025 11:57:44'!
dispatchMouseOverEvent: aMouseEvent
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	| hand mustSetKeyboardFocus |
	hand := aMouseEvent hand.

	"mouseOverMorphs: Array with all morphs that have had a MouseOver event in the previous call.
	leftMorphs is initialized with all morphs that had the mouse over them
	overMorphs and enteredMorphs are initialized empty."
	leftMorphs := mouseOverMorphs asIdentitySet.
	"Assume some coherence for the number of objects in over list"
	overMorphs := WriteStream on: (Array new: leftMorphs size).
	enteredMorphs := WriteStream on: #().
	
	"Now go looking for eventual mouse overs.
	If this MouseOver gets delivered to some morph, it will end calling #noticeMouseOver:event:,
	updating our instance variables.
	leftMorphs will only keep morphs that no longer have the mouse over them.
	enteredMorphs will include morphs that have the mouse over them, and didn't before.
	overMorphs will include all morphs that have the mouse over them."
	aMouseEvent asMouseOver startDispatchFromHand: hand.
	"enteredMorphs and overMorphs are ordered:
	- First is the innest and topmost found morph.
	- Then comes its owner (meaning it is below it), and so on.
	- Last is the World, which is below every other morph."
	
	"Get out early if no change. No morph was entered or left, mouseOverMorphs remains same."
	(leftMorphs isEmpty and: [enteredMorphs position = 0])
		ifTrue: [
			leftMorphs := enteredMorphs := overMorphs := nil.
			^self ].
	enteredMorphs := enteredMorphs contents.
	overMorphs := overMorphs contents.
	
	mustSetKeyboardFocus := Preferences at: #focusFollowsMouse.

	"Send #mouseLeave as appropriate. Release any keyboard focus.
	Order is inner to outer."
	"Keep the order of the left morphs by recreating it from the mouseOverMorphs"
	leftMorphs size > 1 
		ifTrue: [leftMorphs := mouseOverMorphs select: [:m | leftMorphs includes: m]].
	leftMorphs do: [ :m |
		mustSetKeyboardFocus ifTrue: [
			hand releaseKeyboardFocusIf: m ].
		m isInWorld ifTrue: [
			aMouseEvent asMouseLeave sendEventTo: m ]].
	
	"Set keyboard focus to the innermost (i.e. topmost) entered morph that will take it."
	overMorphs do: [ :m |
		m isInWorld ifTrue: [
			(mustSetKeyboardFocus and: [ m handlesKeyboardFocus ]) ifTrue: [
				hand newKeyboardFocus: m.
				"Do it just once!!"
				mustSetKeyboardFocus := false ]]].
	
	"Send #mouseEnter as appropriate. Order is outer to Inner."
	enteredMorphs reverseDo: [ :m |
		m isInWorld ifTrue: [
			aMouseEvent asMouseEnter sendEventTo: m ]].
	
	"And remember the over list for the next call"
	mouseOverMorphs := overMorphs.
	leftMorphs := enteredMorphs := overMorphs := nil.! !


!EntryFieldMorph reorganize!
('accessing' backgroundColor backgroundColor: baseFont contents contents: crAction crAction: disableEditing editor emphasis emphasis: escAction escAction: fitContents font font: fontToUse)
('blink cursor' onBlinkCursor pauseBlinking startBlinking stopBlinking)
('drawing' characterIndexAtPoint: displayTextCursorAtX:top:bottom:emphasis:on: drawOn: drawSelectionOn: drawTextCursorOn: textOffsetdrawTextCursorOn:)
('event handling testing' disablesEditing handlesKeyboard handlesKeyboardFocus handlesMouseDown: handlesMouseOver:)
('events' clickAndHalf:localPosition: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseMove:localPosition:)
('events-processing' fontPreferenceChanged keyboardFocusChange:)
('geometry' measureContents minimumExtent textBox textOffset)
('initialization' defaultBackgroundColor defaultBorderWidth defaultColor initWithContents:font:emphasis: initialize)
('testing' hasSelection hasTextCursor)
('typing/selecting keys' selectAll)
('unaccepted edits' hasUnacceptedEdits:)
('menu - shortcuts' menuNshortcutsSpecsDo:)
('private' installEditor)
!


!HierarchicalListMorph reorganize!
('accessing' autoExpand rejectDrops roots selectedMorph)
('commands' expandAll expandAllAsPer: toggleExpandedState:event:)
('drawing' drawOn:)
('event handling' itemFromPoint: keyboardFocusChange: mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition:)
('events-processing' processMouseMove:localPosition:)
('geometry' scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: autoExpand: currentlyExpanded defaultColor doubleClickSelector: indentingItemClass innerMorphClass list: listItemHeight model:listGetter:indexGetter:indexSetter:)
('keyboard navigation' arrowKey: navigateDown navigateLeft navigateOnePageDown navigateOnePageUp navigateRight navigateToBottom navigateToTop navigateUp setSelectionIndex: toggleExpandedState:)
('model access' getList)
('selection' getCurrentSelectionItem highlightedMorph: listMorphAt: maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: scrollSelectionIntoView selectedMorph: selection: selectionIndex: setSelectedMorph: visualSelectionIndex)
('updating' update:)
('private' addMorphsTo:from:allowSorting:withExpandedItems:atLevel: addSubmorphsAfter:fromCollection:allowSorting: changeSelectionTo: indexForItem: insertNewMorphs: noteRemovalOfAll:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
!


!PluggableListMorph reorganize!
('accessing' rowAtLocation: rowAtLocation:ifNone:)
('accessing - properties' rejectDrops)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
('event handling' keyboardFocusChange: keyboardFocused mouseButton2Activity)
('event handling testing' handlesKeyboard)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition: mouseLeave:)
('events-processing' clickedAt:maybeDeselect: invalidDrop: processMouseMove:localPosition:)
('geometry' desiredExtent desiredHeight fontPreferenceChanged scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: defaultColor doubleClickSelector: font font: initForKeystrokes initialize innerMorphClass model:listGetter:indexGetter:indexSetter:)
('keyboard navigation' arrowKey: deleteAction navigateDown navigateOnePageDown navigateOnePageUp navigateToBottom navigateToTop navigateUp)
('menus' addCustomMenuItems:hand: copyListToClipboard copySelectionToClipboard)
('model access' getCurrentSelectionIndex getList getListItem: getListSize itemSelectedAmongMultiple: keyboardSearch: setSelectionIndex:)
('selection' maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: selection: selectionIndex: visualSelection visualSelectionIndex)
('testing' allowsMultipleSelection is:)
('updating' update: updateList verifyContents)
('private' changeSelectionTo:)
('scrolling' scrollBy: scrollSelectionIntoView scrollSiblings: scrollToShow: updateSiblingsScroller vScrollBarValue:)
('layout' limitLayoutHeight maximumLayoutHeight)
!

