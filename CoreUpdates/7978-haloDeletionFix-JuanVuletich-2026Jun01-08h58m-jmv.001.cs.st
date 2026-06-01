'From Cuis7.9 [latest update: #7978] on 1 June 2026 at 9:03:26 am'!

!HaloMorph methodsFor: 'initialization' stamp: 'jmv 1/Jun/2026 08:58:35'!
delete

	self world ifNotNil: [ :w | w activeHand releaseHalo: self ].
	super delete.
	target ifNotNil: [ target redrawNeeded ].! !


!HandMorph methodsFor: 'halo handling' stamp: 'jmv 1/Jun/2026 09:02:56'!
halo: newHalo
	"Set halo associated with this hand"
	| oldHalo |
	oldHalo := self halo.
	oldHalo == newHalo ifTrue: [
		^self ].
	oldHalo ifNotNil: [ oldHalo delete ].
	newHalo ifNotNil: [ self setProperty: #halo toValue: newHalo ].! !

