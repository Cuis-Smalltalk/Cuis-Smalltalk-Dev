'From Cuis 5.0 [latest update: #4342] on 6 August 2020 at 2:30:57 pm'!

!PasteUpMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 8/6/2020 14:30:22'!
canHandle: aMorph

	^self canvas canDraw: aMorph! !


!WorldState methodsFor: 'canvas' stamp: 'jmv 8/6/2020 14:29:23'!
clearCanvas
	self setCanvas: Display getCanvas.
	damageRecorder _ DamageRecorder new.! !

