'From Cuis 5.0 [latest update: #4597] on 15 May 2021 at 8:32:28 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 5/15/2021 18:45:04'!
fullUpdateCurrentBounds
	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph drawOn: self.
			currentMorph postDrawOn: self. ].
	currentMorph displayBoundsSetFrom: self.
	isKnownFailing ifFalse: [
		currentMorph submorphsDo: [ :m |
			self fullUpdateBounds: m ]]! !

