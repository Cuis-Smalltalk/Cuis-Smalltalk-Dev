'From Cuis 6.0 [latest update: #5772] on 20 April 2023 at 11:31:13 am'!

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 4/18/2023 15:48:30'!
addPragma: aPragma
	self properties: (self properties copyWith: aPragma)! !


!Association methodsFor: 'testing' stamp: 'jmv 4/19/2023 16:17:00'!
literalEqual: otherLiteral
	"Answer true if the receiver and otherLiteral represent the same literal.
	Variable bindings are literally equals only if identical.
	This is how variable sharing works, by preserving identity of the Association, and changing only the value."
	^self == otherLiteral! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/19/2023 14:33:30'!
allClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system."

	self classNames do: [ :name |
		aBlock value: (self at: name) ].! !

