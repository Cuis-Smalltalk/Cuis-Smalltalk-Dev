'From Cuis7.9 [latest update: #8084] on 23 July 2026 at 4:23:49 pm'!

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 7:03:17 pm (UTC)'!
hasLiteralSuchThat: testBlock scanForSpecial: aBoolean
	"Backstop. Avoid bytecode scanning in CompiledCode and below."

	self
		allLiteralsDo: [:literal | (testBlock value: literal) ifTrue: [^ true]]
		scanForSpecial: aBoolean.
	
	^ false! !


!Object methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 7:06:26 pm (UTC)'!
hasLiteral: literal

	^ self hasLiteralSuchThat: [:lit | lit literalEqual: literal]! !

!Object methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 7:05:48 pm (UTC)'!
hasLiteralSuchThat: testBlock
	"Also see #hasLiteral:. Do not scan for implicit (i.e. special) literals by default to get a faster response."

	self allLiteralsDo: [:literal | (testBlock value: literal) ifTrue: [^ true]].
	^ false! !


!CompiledMethod methodsFor: 'literals' stamp: 'jmv 23/Jul/2026, 7:02:43 pm (UTC)'!
allLiteralsDo: aBlock scanForSpecial: aBoolean
	"Overwritten to skip certain (raw) literals."
		
	"Exclude method selector (or properties) and the method's class. Include code literals."
	1 to: self numLiterals - 2 do: [:index |
		(self literalAt: index) allLiteralsDo: aBlock ].

	"Enumerate the implicit literals in bytecodes of the receiver."
	aBoolean ifTrue: [self implicitLiteralsDo: aBlock].

	"Enumerate method selector only through additional method state."
	self
		withPropertiesDo: [ :properties | properties allLiteralsDo: aBlock ]
		ifSelector: [ :selector | nil ].! !

!methodRemoval: CompiledMethod #propertyKeysAndValuesDo: stamp: 'jmv 23/Jul/2026, 7:22:51 pm (UTC)'!
CompiledMethod removeSelector: #propertyKeysAndValuesDo:!
!methodRemoval: CompiledMethod #hasNewPropertyFormat stamp: 'jmv 23/Jul/2026, 7:22:30 pm (UTC)'!
CompiledMethod removeSelector: #hasNewPropertyFormat!
!methodRemoval: Object #hasLiteralSuchThat:scanForSpecial: stamp: 'jmv 23/Jul/2026, 7:21:26 pm (UTC)'!
Object removeSelector: #hasLiteralSuchThat:scanForSpecial:!
!methodRemoval: Object #allLiteralsDo:scanForSpecial: stamp: 'jmv 23/Jul/2026, 7:22:17 pm (UTC)'!
Object removeSelector: #allLiteralsDo:scanForSpecial:!
