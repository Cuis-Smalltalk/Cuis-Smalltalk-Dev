'From Cuis 5.0 of 7 November 2016 [latest update: #3632] on 25 February 2019 at 3:37:56 pm'!

!PluggableListMorph methodsFor: 'events' stamp: 'jmv 2/25/2019 15:21:30'!
dragEvent: aMouseEvent localPosition: localEventPosition

	| row dragged listItem |
	self listMorph highlightedRow: nil.
	row _ self rowAtLocation: localEventPosition ifNone: [ ^self ].
	listItem _ self getListItem: row.

	dragged _ DraggingGuideMorph new.
	dragged addMorph: (StringMorph contents: listItem).
	dragged setProperty: #dragSource toValue: self.
	dragged setProperty: #dropSelectorArgument toValue: listItem.
	
	aMouseEvent hand attachMorphBeside: dragged.! !

