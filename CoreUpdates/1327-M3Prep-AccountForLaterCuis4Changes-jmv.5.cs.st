'From Cuis 4.0 of 3 April 2012 [latest update: #1261] on 10 April 2012 at 3:51:44 pm'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 1/6/2012 15:31'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	self validatePositionAndBounds.
	self validateExtentAndBounds.
	newExtent _ (self paragraph extent max: 9 @ StrikeFont default height) + (0 @ 2).
	extent = newExtent ifFalse: [
		self basicExtent: newExtent ].

	self redrawNeeded.	"Too conservative: only paragraph composition
							should cause invalidation."
	owner innerHeight: newExtent y! !


!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 1/6/2012 14:52'!
morphExtent: newExtent
	
	| minH minW |
	"Figure out the minimum width and height for this pane so that scrollbars will appear"
	minH _ self vIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	minW _ self hIsScrollbarShowing
		ifTrue: [self scrollBarClass scrollbarThickness * 2]
		ifFalse: [0].
	super morphExtent: (newExtent max: (minW@minH)).

	"Now reset widget sizes"
	scroller adjustExtent.
	self updateScrollBarsBounds.
	self setScrollDeltas! !

!methodRemoval: StringMorph #setWidth:!
StringMorph removeSelector: #setWidth:!
