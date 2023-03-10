'From Cuis 6.0 [latest update: #5682] on 10 March 2023 at 4:28:01 pm'!

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 3/10/2023 16:27:02'!
setScrollDeltas
	"Set the ScrollBar deltas, value and interval, based on the current scroll pane size, offset and range."

	self hideOrShowScrollBars.
	self vIsScrollbarShowing ifTrue: [ self vSetScrollDelta ].
	self hIsScrollbarShowing ifTrue: [ self hSetScrollDelta ].! !

