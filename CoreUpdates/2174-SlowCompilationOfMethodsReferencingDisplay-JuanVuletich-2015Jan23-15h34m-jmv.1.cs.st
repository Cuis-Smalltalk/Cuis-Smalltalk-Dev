'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 23 January 2015 at 3:35:00.128177 pm'!

!Form methodsFor: 'comparing' stamp: 'jmv 1/23/2015 15:29'!
= other 
	"Compare for equal contents. Expensive!!
	See comment at #hash"

	^ (other class == self class) 
		and: [ other width = width
			and: [ other height = height
				and: [ other depth = depth
					and: [ other offset = offset
						and: [ other bits = bits ]]]]]! !

!Form methodsFor: 'comparing' stamp: 'jmv 1/23/2015 15:33'!
hash
	"Hash is re-implemented because #= is re-implemented.
	But it is expensive.

	Storing (sub)instances of Form in Sets, or using them as keys in Dictionaries is 
	usually not a good idea, because #= and #hash are expensive, and because Forms
	are usually mutable.
	Maybe use an IdentitySet or IdentityDictionary instead?"

	^bits hash! !


!Encoder methodsFor: 'initialization' stamp: 'jmv 1/23/2015 15:24'!
initScopeAndLiteralTables

	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := IdentityDictionary new: 16.
	literalStream := WriteStream on: (Array new: 32).
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new! !

