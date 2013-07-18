'From Cuis 4.1 of 12 December 2012 [latest update: #1549] on 6 January 2013 at 8:46:26 pm'!

!PluggableListMorph methodsFor: 'selection' stamp: 'jmv 1/15/2013 17:18'!
scrollSelectionIntoView
	"make sure that the current selection is visible"
	| row r |
	row _ self getCurrentSelectionIndex.
	row = 0 
		ifTrue: [
			"Value is 0, but we need to propagate it to model"
			scrollBar setValue: scrollBar value ]
		ifFalse: [
			self flag: #jmvVer2.
			r _ self listMorph drawBoundsForRow: row.
			r _ ((self listMorph externalize: r origin) extent: r extent).
			self scrollToShow: r ]! !


