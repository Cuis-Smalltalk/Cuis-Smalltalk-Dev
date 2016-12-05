'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 26 November 2016 at 8:46:53 am'!

!MenuMorph methodsFor: 'keyboard control' stamp: 'len 6/11/2016 20:40'!
keyboardFocusChange: aBoolean
	"Notify change due to green border for keyboard focus"

	aBoolean ifFalse: [self deleteIfPopUp: nil].
	self redrawNeeded! !

