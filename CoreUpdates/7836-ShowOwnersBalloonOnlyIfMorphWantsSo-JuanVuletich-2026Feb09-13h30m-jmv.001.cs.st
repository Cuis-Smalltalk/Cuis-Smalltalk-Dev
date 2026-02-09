'From Cuis7.7 [latest update: #7835] on 9 February 2026 at 1:36:08 pm'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/Feb/2026 13:33:00'!
wantsOwnersBalloon
	"Answer true if receiver wants to show owners balloon help,
	if we don't #wantsBalloon ourselves."

	^ false! !


!DraggableButtonMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/Feb/2026 13:34:26'!
wantsOwnersBalloon
	"Answer true if receiver wants to show owners balloon help,
	if we don't #wantsBalloon ourselves."

	^ true! !


!ScrollBar methodsFor: 'halos and balloon help' stamp: 'jmv 9/Feb/2026 13:34:45'!
wantsOwnersBalloon
	"Answer true if receiver wants to show owners balloon help,
	if we don't #wantsBalloon ourselves."

	^ true! !


!HandMorph methodsFor: 'balloon help' stamp: 'jmv 9/Feb/2026 13:32:23'!
scheduleBalloonFor: aMorph
	"Trigger balloon help after the given time out for some morph."
	
	"Our own. #showBalloon, #balloonText"
	aMorph wantsBalloon ifTrue: [
		^self addAlarm: #spawnBalloonFor: with: aMorph after: self balloonHelpDelayTime ].

	"Our owners"
	aMorph wantsOwnersBalloon ifTrue: [
		aMorph owner ifNotNil: [ :itsOwner |
			self scheduleBalloonFor: itsOwner ]].! !

