'From Cuis 4.0 of 21 April 2012 [latest update: #1292] on 25 May 2012 at 11:02:52 pm'!

!IndentingListItemMorph methodsFor: 'drawing' stamp: 'jmv 5/25/2012 23:02'!
unhighlight

	complexContents highlightingColor ifNotNil: [ self color: Theme current text ].
	self redrawNeeded! !

