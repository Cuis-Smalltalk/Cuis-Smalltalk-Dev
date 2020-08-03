'From Cuis 5.0 [latest update: #4329] on 3 August 2020 at 2:34:23 pm'!

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 8/3/2020 14:31:56'!
dragEvent: aMouseEvent localPosition: localEventPosition

	| row dragged listItem sm |
	self listMorph highlightedRow: nil.
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem _ self getListItem: row.
	
	sm _ StringMorph contents: listItem.
	dragged _ DraggingGuideMorph new.
	dragged addMorph: sm.
	dragged morphExtent: sm morphExtent.
	dragged setProperty: #dragSource toValue: self.
	dragged setProperty: #dropSelectorArgument toValue: listItem.
	
	aMouseEvent hand attachMorphBeside: dragged.! !

