'From Cuis 5.0 of 7 November 2016 [latest update: #3211] on 14 November 2017 at 2:06:38 pm'!

!DisplayScreen methodsFor: 'other' stamp: 'jmv 11/14/2017 14:04:10'!
restoreAfter: aBlock
	"
	- Evaluate the block
	- Update host OS Display
	- Wait for a mouse click
	- And then restore the Morphic World"

	aBlock value.
	self forceToScreen.
	Sensor waitButton.
	self runningWorld ifNotNil: [ :w | w fullRepaintNeeded ]! !

