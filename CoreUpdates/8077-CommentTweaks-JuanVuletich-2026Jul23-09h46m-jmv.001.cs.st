'From Cuis7.9 [latest update: #8076] on 23 July 2026 at 9:47:16 am'!

!Object methodsFor: 'literals' stamp: 'mt 15/Dec/2023, 1:22:00 pm (UTC)'!
allLiteralsDo: aBlock scanForSpecial: aBoolean
	"Backstop. Avoid bytecode scanning in CompiledCode and below. See #hasLiteralSuchThat:scanForSpecial:."
	
	self allLiteralsDo: aBlock.! !

!Object methodsFor: 'literals' stamp: 'mt 15/Dec/2023, 1:23:00 pm (UTC)'!
hasLiteralSuchThat: testBlock scanForSpecial: aBoolean
	"Backstop. Avoid bytecode scanning in CompiledCode and below."

	self
		allLiteralsDo: [:literal | (testBlock value: literal) ifTrue: [^ true]]
		scanForSpecial: aBoolean.
	
	^ false! !

!Object methodsFor: 'testing' stamp: 'ar 15/Aug/2001, 2:19:00 am (UTC)'!
isVariableBinding
	"Return true if I can behave as a literal variable binding in CompiledMethods. See senders."
	^false! !


!LookupKey methodsFor: 'testing' stamp: 'ar 15/Aug/2001, 1:39:00 am (UTC)'!
isVariableBinding
	"Return true if I can behave as a literal variable binding in CompiledMethods. See senders."
	^true! !

