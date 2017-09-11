'From Cuis 5.0 of 7 November 2016 [latest update: #3163] on 31 August 2017 at 5:46:33 am'!

!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'pb 8/31/2017 05:46:22'!
arrowKey: aChar
	"Handle a keyboard navigation character. Answer true if handled, false if not."
	| keyEvent answer nextSelection oldSelection |
	answer := false.
	keyEvent := aChar numericValue.
	oldSelection := self getSelectionIndex.
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
					answer := true ]
				ifFalse: [ nextSelection := oldSelection + 1 ]]].
	keyEvent = 28 ifTrue: [
		"left"
		selectedMorph ifNotNil: [
			selectedMorph isExpanded
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					answer := true ]
				ifFalse: [ nextSelection := oldSelection - 1 max: 1 ]].
		^ true ].
	nextSelection = oldSelection ifFalse: [
		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).
		"Update the model in next world cycle, so user gets the immediate feedback."
		WorldState addDeferredUIMessage: [ self setSelectionIndex: nextSelection ].
		answer := true ].
	^ answer.! !

