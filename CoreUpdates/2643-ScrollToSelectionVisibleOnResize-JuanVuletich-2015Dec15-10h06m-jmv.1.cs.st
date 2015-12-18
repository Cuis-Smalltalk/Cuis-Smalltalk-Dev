'From Cuis 4.2 of 25 July 2013 [latest update: #2633] on 15 December 2015 at 12:43:50.204078 pm'!

!PluggableScrollPane methodsFor: 'scrolling' stamp: 'jmv 12/15/2015 12:42'!
scrollSelectionIntoView
	"Scroll my text into view if necessary and return true, else return false"
! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 12/15/2015 12:43'!
privateExtent: aPoint

	^ (super privateExtent: aPoint)
		ifTrue: [
			"Now reset widget sizes"
			scroller adjustExtent.
			self updateScrollBarsBounds.
			self setScrollDeltas.
			self scrollSelectionIntoView ]; yourself! !

