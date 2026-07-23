'From Cuis7.9 [latest update: #8078] on 23 July 2026 at 11:32:59 am'!

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 1:40:57 pm (UTC)'!
allLiteralsDo: aBlock scanForSpecial: aBoolean
	"Overwritten to skip certain (raw) literals."
		
	"Exclude method selector (or properties) and the method's class. Include code literals."
	1 to: self numLiterals - 2 do: [:index |
		(self literalAt: index) allLiteralsDo: aBlock scanForSpecial: aBoolean].

	"Enumerate the implicit literals in bytecodes of the receiver."
	aBoolean ifTrue: [self implicitLiteralsDo: aBlock].

	"Enumerate method selector only through additional method state."
	self
		withPropertiesDo: [ :properties |
			properties allLiteralsDo: aBlock scanForSpecial: aBoolean]
		ifSelector: [ :selector | nil ].! !

