'From Cuis7.1 [latest update: #6643] on 27 August 2024 at 2:58:30 pm'!

!WorldMorph methodsFor: 'hands' stamp: 'jmv 8/27/2024 14:08:41'!
activeHand
	activeHand isNil ifTrue: [
		hands isEmpty
			ifTrue: [
				activeHand := HandMorph new.
				hands := {activeHand } ]
			ifFalse: [ activeHand := hands first].].
	^activeHand.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/27/2024 14:07:15'!
doOneMinimalCycleNow
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorldOn: self mainCanvas.

	"Process user input events. Run all event triggered code."
	^self activeHand processEventQueue! !

