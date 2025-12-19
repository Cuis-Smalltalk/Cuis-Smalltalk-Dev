'From Cuis7.5 [latest update: #7772] on 19 December 2025 at 10:48:04 am'!

!PluggableListMorph methodsFor: 'updating' stamp: 'jmv 19/Dec/2025 10:08:28'!
updateList
	| index |
	"the list has changed -- update from the model"
	self getList.
	scroller listChanged.
	self setScrollDeltas.
	index := self getCurrentSelectionIndex.
	self privateVisualSelectionIndex: index.
	self redrawNeeded.! !

