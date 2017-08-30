'From Cuis 5.0 of 7 November 2016 [latest update: #3130] on 27 July 2017 at 2:24:40 pm'!

!HierarchicalListMorph methodsFor: 'commands' stamp: 'pb 7/27/2017 14:24:23'!
toggleExpandedState: aMorph event: event

	"self setSelectedMorph: aMorph."
	((self autoExpand or: [event shiftPressed]) and: [aMorph isExpanded not])
		ifTrue: [aMorph beFullyExpanded]
		ifFalse: [aMorph toggleExpandedState].
	scroller adjustExtent.
	self setScrollDeltas! !

