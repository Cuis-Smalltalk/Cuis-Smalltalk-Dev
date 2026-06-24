'From Cuis7.9 [latest update: #8017] on 24 June 2026 at 3:47:31 pm'!

!TaskbarMorph methodsFor: 'services' stamp: 'jmv 24/Jun/2026, 5:37:06 pm (UTC)'!
wasOpened: aMorph
	"aMorph was added to the world. Add button for aMorph if appropriate."

	aMorph isIncludedInTaskbar
		ifTrue: [
			(self buttonFor: aMorph) ifNil: [self addButtonFor: aMorph ]]! !

!methodRemoval: WorldMorph #removedMorph: stamp: 'jmv 24/Jun/2026, 6:45:45 pm (UTC)'!
WorldMorph removeSelector: #removedMorph:!
