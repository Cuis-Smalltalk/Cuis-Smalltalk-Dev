'From Cuis 4.1 of 12 December 2012 [latest update: #1660] on 3 April 2013 at 8:53:37 am'!

!WorldState methodsFor: 'canvas' stamp: 'jmv 4/3/2013 08:52'!
clearCanvas
	canvas _ nil.
	damageRecorder _ DamageRecorder new.
	damageRecorder doFullRepaint! !

!WorldState methodsFor: 'canvas' stamp: 'jmv 4/3/2013 08:52'!
setCanvas: aFormCanvas
	canvas _ aFormCanvas.
	canvas into: world.
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new].
	damageRecorder doFullRepaint! !

