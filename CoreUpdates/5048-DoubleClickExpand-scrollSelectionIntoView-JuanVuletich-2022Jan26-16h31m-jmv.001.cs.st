'From Cuis 6.0 [latest update: #5047] on 26 January 2022 at 4:33:58 pm'!

!HierarchicalListMorph methodsFor: 'events' stamp: 'jmv 1/26/2022 16:31:14'!
doubleClick: aMouseButtonEvent localPosition: localEventPosition
	doubleClickSelector ifNotNil: [^ self model perform: doubleClickSelector].
	selectedMorph ifNotNil: [
		self toggleExpandedState: selectedMorph.
		self scrollSelectionIntoView ]! !

