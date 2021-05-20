'From Cuis 5.0 [latest update: #4601] on 16 May 2021 at 10:30:50 am'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 5/15/2021 20:34:13'!
fullUpdateCurrentBounds
	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph drawOn: self.
			currentMorph postDrawOn: self ].
	currentMorph displayBoundsSetFrom: self.
	isKnownFailing ifFalse: [
		currentMorph submorphsDo: [ :m |
			self fullUpdateBounds: m ]]! !

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 5/15/2021 20:46:17'!
updateHandsDisplayBounds: aWorldMorph
	"Update displayBounds for carried morphs if never set.
	Useful for new morph, that are created and attached to the hand."
	
	aWorldMorph handsDo: [ :hand |
		hand hasSubmorphs ifTrue: [
			self into: hand.
			hand submorphsDo: [ :morph |
				morph displayBounds ifNil: [
					self fullUpdateBounds: morph ]].
			self outOfMorph ]]! !

!methodRemoval: MorphicCanvas #updateCurrentDisplayBoundsAndMerge: stamp: 'jmv 5/16/2021 10:29:05'!
MorphicCanvas removeSelector: #updateCurrentDisplayBoundsAndMerge:!
!methodRemoval: MorphicCanvas #updateDisplayBounds:andMerge: stamp: 'jmv 5/16/2021 10:29:03'!
MorphicCanvas removeSelector: #updateDisplayBounds:andMerge:!
