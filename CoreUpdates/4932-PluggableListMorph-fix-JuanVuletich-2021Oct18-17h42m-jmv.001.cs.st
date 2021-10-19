'From Cuis 5.0 [latest update: #4934] on 18 October 2021 at 5:42:54 pm'!

!PluggableListMorph methodsFor: 'initialization' stamp: 'jmv 10/18/2021 17:18:08'!
initialize
	scrollSiblings := false. "user must override"
	super initialize.
	scroller morphWidth: extent x.! !

