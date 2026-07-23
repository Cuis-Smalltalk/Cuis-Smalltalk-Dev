'From Cuis7.9 [latest update: #8077] on 23 July 2026 at 10:05:05 am'!

!Association methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 12:34:50 pm (UTC)'!
allLiteralsDo: aBlock

	key allLiteralsDo: aBlock.
	value allLiteralsDo: aBlock.! !


!Ephemeron methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 12:38:51 pm (UTC)'!
allLiteralsDo: aBlock
	"This method is never executed, as Ephemerons are not used in CompiledMethod properties.
	Sill, provide correct behavior: do nothing."! !


!ReadOnlyVariableBinding methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 12:35:08 pm (UTC)'!
allLiteralsDo: aBlock

	key allLiteralsDo: aBlock.
	value allLiteralsDo: aBlock.! !


!Object methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 12:42:21 pm (UTC)'!
allLiteralsDo: aBlock
	"Enumerate all literals in the receiver. See inheritance."

	self isLiteral ifTrue: [
		aBlock value: self ].! !


!Array methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 12:30:48 pm (UTC)'!
allLiteralsDo: aBlock

	"If I am a literal."
	self isLiteral ifTrue: [
		aBlock value: self ].
	
	"All my elements that are literals."
	self do: [:each | each allLiteralsDo: aBlock].! !


!LookupKey methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 1:04:12 pm (UTC)'!
allLiteralsDo: aBlock

	key allLiteralsDo: aBlock.! !

