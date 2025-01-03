'From Cuis7.3 [latest update: #6963] on 3 January 2025 at 4:22:57 pm'!

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 1/3/2025 16:22:52'!
recordDamagedRect: damageRect for: aMorph

	damageRect hasPositiveExtent ifFalse: [ ^self ].

	damageRecorder ifNotNil: [
		damageRecorder recordInvalidRect: damageRect for: aMorph ].! !

