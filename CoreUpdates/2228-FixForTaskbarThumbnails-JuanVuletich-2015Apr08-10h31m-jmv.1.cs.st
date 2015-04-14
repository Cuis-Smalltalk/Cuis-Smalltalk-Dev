'From Cuis 4.2 of 25 July 2013 [latest update: #2226] on 8 April 2015 at 10:41:53.988769 am'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2015 10:39'!
displayBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph when drawn on our form.
	Might be larger than strictly required.
		- In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because:
			a) Morphic 2 doesn't use scaling and rotation
			b) Most Morphic 2 morphs have rectangular shape.
		- In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts."

	"Think about doing a BoundsFinderCanvas even for Morphic 2"
	self flag: #jmvVer2.

	^self externalizeDisplayBounds: aMorph morphLocalBounds from: aMorph! !

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2015 10:39'!
displayFullBoundsInWorldOf: aMorph
	"Answer a rectangle that completely bounds aMorph and submorphs when drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Might be larger than strictly required. See comment at #displayBoundsInWorldOf:"

	| r |
	"Think about doing a BoundsFinderCanvas even for Morphic 2"

	self flag: #jmvVer2.

	r _ self displayBoundsInWorldOf: aMorph.
	aMorph unclippedSubmorphsReverseDo: [ :m |
		m visible ifTrue: [
			r _ r merge: (self displayFullBoundsInWorldOf: m) ]].

	^r! !


!Morph methodsFor: 'geometry' stamp: 'jmv 4/8/2015 10:39'!
morphBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."

	self flag: #jmvVer2.
	self visible ifFalse: [ ^nil ].
	^self world ifNotNil: [ :w | w canvas ifNotNil: [ :c | c displayBoundsInWorldOf: self ]]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 4/8/2015 10:36'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."

	self flag: #jmvVer2.
	self visible ifFalse: [ ^nil ].
	^self world ifNotNil: [ :w | w canvas displayFullBoundsInWorldOf: self ]! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 4/8/2015 10:32'!
clippingRectForCurrentMorph
	"In form coordinates"

"En M3, es el obtenido al dibujarlo, recien.
Dejar eso claro en el nombre. Eliminar 'clipping'"
	^currentTransformation displayBoundsOfTransformOf: currentMorph morphLocalBounds! !

!methodRemoval: MorphicCanvas #displayBoundsOf:!
MorphicCanvas removeSelector: #displayBoundsOf:!
!methodRemoval: MorphicCanvas #displayFullBoundsOf:!
MorphicCanvas removeSelector: #displayFullBoundsOf:!
