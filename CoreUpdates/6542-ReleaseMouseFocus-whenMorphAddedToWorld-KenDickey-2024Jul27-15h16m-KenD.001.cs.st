'From Cuis7.1 [latest update: #6538] on 27 July 2024 at 3:17:32 pm'!

!WorldMorph methodsFor: 'change reporting' stamp: 'KenD 7/27/2024 07:32:54'!
addedMorph: aMorph
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self firstHand newMouseFocus: nil. "Hand need to reset mouseFocus"
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]! !

