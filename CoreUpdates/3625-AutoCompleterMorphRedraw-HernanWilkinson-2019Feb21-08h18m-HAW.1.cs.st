'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 21 February 2019 at 3:10:53 pm'!

!AutoCompleterMorph methodsFor: 'actions' stamp: 'HAW 2/21/2019 11:51:45'!
resetMenu
	
	| width newExtent adjustedY |
	
	self hideSelectorDocumentation.
	firstVisible _ 1.
	self selected: 1.

	width _ self calculateWidth.
	newExtent _ width + 4 @ (self visibleItemsCount * self itemHeight+2).
	
	adjustedY := (self isYOutOfScreen: originalPosition with: newExtent) 
		ifTrue: [ originalPosition y - newExtent y - self itemHeight ]
		ifFalse: [ originalPosition y ].

	self morphPosition: originalPosition x @ adjustedY extent: newExtent.
	"redraw is needed even if position and extent haven't changed"
	self redrawNeeded ! !

