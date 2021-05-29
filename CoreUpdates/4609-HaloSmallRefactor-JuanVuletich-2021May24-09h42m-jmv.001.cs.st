'From Cuis 5.0 [latest update: #4608] on 24 May 2021 at 9:47:26 am'!

!WorldMorph methodsFor: 'world state' stamp: 'jmv 5/24/2021 09:45:48'!
haloMorphsDo: aBlock
	self handsDo: [ :hand | hand halo ifNotNil: [ :halo | aBlock value: halo ]].! !


!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 5/24/2021 09:44:42'!
halo
	self world ifNotNil: [ :w |
		w haloMorphsDo: [ :h |
			h target == self ifTrue: [^ h]]].
	^ nil! !


!WorldMorph methodsFor: 'world state' stamp: 'jmv 5/24/2021 09:45:30'!
deleteAllHalos
	| c |
	c _ OrderedCollection new.
	self haloMorphsDo: [ :halo | c add: halo ].
	self removeAllMorphsIn: c! !

!methodRemoval: WorldMorph #haloMorphs stamp: 'jmv 5/24/2021 09:46:22'!
WorldMorph removeSelector: #haloMorphs!
