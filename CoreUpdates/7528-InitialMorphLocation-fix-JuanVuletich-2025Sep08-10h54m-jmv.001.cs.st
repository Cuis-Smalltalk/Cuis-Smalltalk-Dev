'From Cuis7.5 [latest update: #7527] on 8 September 2025 at 10:56:46 am'!

!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/8/2025 10:50:42'!
center: aMorph near: aPoint
	"Position the given morph in this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	aMorph morphExtent
		ifNil: [ aMorph positionInWorld: aPoint ]
		ifNotNil:
			[ :e |
				| trialRect delta |
				trialRect := Rectangle center: aPoint extent: e.
				delta := trialRect amountToTranslateWithin: self displayBounds.
				aMorph positionInWorld: (trialRect origin + delta) rounded ].! !


!PlacedMorph methodsFor: 'initialization' stamp: 'jmv 9/8/2025 10:56:28'!
openInWorld: aWorld
	"Add this morph to the requested World."

	aWorld addMorph: self.
	
	"We could have already set our location, or maybe #noteNewOwner: was redefined to set it.
	Position us near the hand only if we haven't been located somewhere already."
	(location isIdentity or: [ location isPureMirroring ])
		ifTrue: [ "No real position set yet"
			aWorld center: self near: aWorld activeHand morphPosition ].! !


!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/8/2025 10:50:55'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	self addMorph: aMorph.
	self center: aMorph near: aPoint.! !

