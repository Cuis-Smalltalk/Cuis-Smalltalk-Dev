'From Cuis 5.0 of 7 November 2016 [latest update: #3656] on 12 March 2019 at 3:09:32 pm'!

!AutoCompleterMorph methodsFor: 'actions' stamp: 'jmv 3/12/2019 15:07:50'!
resetMenu

	| width newExtent |
	self hideSelectorDocumentation.

	firstVisible := 1.
	self selected: ((completer canSelect: (completer entries at: firstVisible)) ifTrue: [firstVisible] ifFalse: [firstVisible+1]).

	self calculateItemsPerPage.
	width _ self calculateWidth.
	newExtent _ width + 4 @ (itemsPerPage * self itemHeight + 2).

	self morphPosition: originalPosition extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded ! !

