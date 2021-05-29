'From Cuis 5.0 [latest update: #4612] on 26 May 2021 at 2:48:21 pm'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 5/26/2021 14:45:18'!
allSubmorphsDo: aBlock
	"Evaluate the given block for all morphs in this composite morph (including the receiver)."

	submorphs do: [ :m |
		aBlock value: m.
		m allSubmorphsDo: aBlock].! !


!Morph methodsFor: 'geometry' stamp: 'jmv 5/26/2021 14:45:29'!
adjustDisplayBoundsBy: delta
	"Private for framework use.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us."

	privateDisplayBounds ifNotNil: [
		privateDisplayBounds _ privateDisplayBounds translatedBy: delta ].
	self allSubmorphsDo: [ :m | m adjustDisplayBoundsBy: delta ].! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 5/26/2021 14:46:46'!
allMorphsDo: aBlock
	"Evaluate the given block for all morphs in this composite morph (including the receiver)."

	aBlock value: self.
	self allSubmorphsDo: aBlock! !

