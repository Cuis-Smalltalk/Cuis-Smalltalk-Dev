'From Cuis7.5 [latest update: #7660] on 15 October 2025 at 8:03:07 pm'!

!HierarchicalListMorph methodsFor: 'testing' stamp: 'jmv 10/15/2025 19:58:59'!
is: aSymbol
	^ aSymbol == #ListMorph or: [ super is: aSymbol ]! !


!DraggingGuideMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 10/15/2025 20:01:01'!
wantsToBeDroppedInto: aMorph
	"Return true if it's okay to drop the receiver into aMorph.
	This check is symmetric to #wantsDroppedMorph:event: to give both parties a chance of figuring out
	whether they like each other."

	^(aMorph is: #ListMorph)
		and: [ (self valueOfProperty: #dragSource) = (aMorph valueOfProperty: #acceptedDragSource) ]! !


!PluggableListMorph methodsFor: 'testing' stamp: 'jmv 10/15/2025 19:58:19'!
is: aSymbol
	^ aSymbol == #PluggableListMorph or: [ aSymbol == #ListMorph or: [ super is: aSymbol ] ]! !


!HierarchicalListMorph reorganize!
('accessing' autoExpand rejectDrops roots selectedMorph)
('commands' expandAll expandAllAsPer: toggleExpandedState:event:)
('drawing' drawOn:)
('event handling' itemFromPoint: keyboardFocusChange: mouseButton2Activity)
('event handling testing' clickToFocusConsumesEvent)
('events' doubleClick:localPosition: dragEvent:localPosition: focusKeyboardFor: keyStroke: mouseButton1Down:localPosition: mouseButton1Up:localPosition:)
('events-processing')
('geometry' scrollDeltaHeight scrollDeltaWidth someSubmorphPositionOrExtentChanged)
('initialization' autoDeselect: autoExpand: currentlyExpanded defaultColor doubleClickSelector: indentingItemClass innerMorphClass list: listItemHeight model:listGetter:indexGetter:indexSetter:)
('keyboard navigation' arrowKey: navigateDown navigateLeft navigateOnePageDown navigateOnePageUp navigateRight navigateToBottom navigateToTop navigateUp setSelectionIndex: toggleExpandedState:)
('model access' getList)
('selection' getCurrentSelectionItem highlightedMorph: listMorphAt: maximumSelection minimumSelection numSelectionsInView privateVisualSelection: privateVisualSelectionIndex: scrollSelectionIntoView selectedMorph: selection: selectionIndex: setSelectedMorph: visualSelectionIndex)
('updating' update:)
('private' addMorphsTo:from:allowSorting:withExpandedItems:atLevel: addSubmorphsAfter:fromCollection:allowSorting: changeSelectionTo: indexForItem: insertNewMorphs: noteRemovalOfAll:)
('testing' is:)
('dropping/grabbing' acceptDroppingMorph:event: acceptDropsFrom:performing: allowItemDrag: wantsDroppedMorph:event:)
!

