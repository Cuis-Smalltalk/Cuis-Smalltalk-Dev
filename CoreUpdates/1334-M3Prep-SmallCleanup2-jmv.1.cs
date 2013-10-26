'From Cuis 4.0 of 3 April 2012 [latest update: #4211] on 12 April 2012 at 9:04:10 am'!

!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 4/12/2012 09:01'!
addToScroller: aMorph

	scroller addMorph: aMorph.
	aMorph morphPositionInOwner: 0@0! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'jmv 4/12/2012 09:03'!
scrollerOffset: newOffset
	| delta |
	delta _ borderWidth + self xtraBorder.
	scroller morphPositionInOwner: delta@delta - newOffset! !

