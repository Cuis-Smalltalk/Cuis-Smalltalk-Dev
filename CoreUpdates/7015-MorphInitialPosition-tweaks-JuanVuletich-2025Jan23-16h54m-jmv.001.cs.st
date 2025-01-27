'From Cuis7.3 [latest update: #7004] on 23 January 2025 at 4:55:27 pm'!

!PlacedMorph methodsFor: 'initialization' stamp: 'jmv 1/23/2025 15:13:48'!
openInWorld: aWorld
	"Add this morph to the requested World."

	(location isIdentity or: [ location isPureMirroring ])
		ifTrue: [ "No real position set yet"
			aWorld addMorph: self centeredNear: aWorld activeHand morphPosition.
			^self ].

	aWorld addMorph: self.! !


!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 1/23/2025 15:12:39'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	self addMorph: aMorph.
	aMorph morphExtent
		ifNil: [ aMorph positionInWorld: aPoint ]
		ifNotNil:
			[ :e |
				| trialRect delta |
				trialRect := Rectangle center: aPoint extent: e.
				delta := trialRect amountToTranslateWithin: self displayBounds.
				aMorph positionInWorld: (trialRect origin + delta) rounded ].! !

