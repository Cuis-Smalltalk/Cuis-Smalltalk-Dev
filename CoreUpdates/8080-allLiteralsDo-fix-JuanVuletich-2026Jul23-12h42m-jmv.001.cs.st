'From Cuis7.9 [latest update: #8079] on 23 July 2026 at 12:45:45 pm'!

!Association methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 3:44:48 pm (UTC)'!
allLiteralsDo: aBlock
	"Treat myself as literal. Used, for instance, to find references to a class. See #referencesToIt"
	aBlock value: self.

	key allLiteralsDo: aBlock.
	value allLiteralsDo: aBlock.! !


!ReadOnlyVariableBinding methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 3:45:25 pm (UTC)'!
allLiteralsDo: aBlock
	"Treat myself as literal. Used, for instance, to find references to a class. See #referencesToIt"
	aBlock value: self.

	key allLiteralsDo: aBlock.
	value allLiteralsDo: aBlock.! !

