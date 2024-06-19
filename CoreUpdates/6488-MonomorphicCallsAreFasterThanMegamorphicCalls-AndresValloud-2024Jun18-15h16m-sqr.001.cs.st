'From Cuis7.1 [latest update: #6483] on 18 June 2024 at 4:03:41 pm'!

!CompiledMethod methodsFor: 'literals' stamp: 'sqr 6/18/2024 15:36:24'!
hasLiteral: literal
	"Answer whether the receiver references the argument, literal."
	2 to: self numLiterals - 1 do: "exclude superclass + selector/properties"
		[:index |
		(literal literalEqual: (self objectAt: index)) ifTrue: [^true]].
	^false! !

